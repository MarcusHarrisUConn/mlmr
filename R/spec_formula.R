model_variables <- function(spec) {
  unique(c(
    spec$outcome,
    names(spec$fixed),
    unlist(spec$grouping, use.names = FALSE),
    unlist(spec$interactions, use.names = FALSE)
  ))
}

is_gaussian_spec <- function(spec) {
  identical(spec$distribution, "gaussian")
}

is_gaussian_fit <- function(fit) {
  inherits(fit, "lmerMod") && !inherits(fit, "glmerMod")
}

family_link_supported <- function(distribution, link) {
  supported <- switch(
    distribution,
    gaussian = "identity",
    binomial = c("logit", "probit"),
    poisson = "log",
    `negative binomial` = "log",
    Gamma = c("log", "inverse"),
    character()
  )
  link %in% supported
}

family_link_detail <- function(distribution) {
  switch(
    distribution,
    gaussian = "Gaussian models use the identity link in the standard workflow.",
    binomial = "Binomial models currently support logit and probit links.",
    poisson = "Poisson models currently support the log link.",
    `negative binomial` = "Negative-binomial models currently use the log link.",
    Gamma = "Gamma models currently support log and inverse links.",
    "Review the selected distribution and link."
  )
}

mlm_spec <- function(outcome, distribution = "gaussian", link = "identity", fixed = list(),
                     grouping = list(), nesting = NULL, structure = "nested", random = list(),
                     interactions = list(), predictor_levels = list(), data = NULL) {
  if (!is.character(outcome) || length(outcome) != 1 || is.na(outcome) || !nzchar(outcome)) {
    stop("`outcome` must be a single variable name.", call. = FALSE)
  }
  supported_distributions <- c("gaussian", "binomial", "poisson", "negative binomial", "Gamma")
  if (!is.character(distribution) || length(distribution) != 1 || !distribution %in% supported_distributions) {
    stop(
      "Unsupported distribution: ", paste(format(distribution), collapse = ", "),
      ". Choose one of: ", paste(supported_distributions, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!is.character(link) || length(link) != 1 || !nzchar(link)) {
    stop("`link` must be a single link-function name.", call. = FALSE)
  }
  if (!is.list(fixed) || !is.list(grouping) || !is.list(random) ||
      !is.list(interactions) || !is.list(predictor_levels)) {
    stop("`fixed`, `grouping`, `random`, `interactions`, and `predictor_levels` must be lists.", call. = FALSE)
  }
  if (length(fixed) && (is.null(names(fixed)) || any(!nzchar(names(fixed))))) {
    stop("`fixed` must be a named list with one entry per predictor, such as list(ses = list(center = \"CWC\")).", call. = FALSE)
  }
  if (length(random) && (is.null(names(random)) || any(!nzchar(names(random))))) {
    stop("`random` must be a named list with one entry per grouping factor.", call. = FALSE)
  }
  if (!is.null(data) && !is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.null(data)) {
    selected <- unique(c(
      outcome,
      names(fixed),
      unlist(grouping, use.names = FALSE),
      unlist(interactions, use.names = FALSE)
    ))
    missing_selected <- setdiff(selected, names(data))
    if (length(missing_selected)) {
      stop("The following selected variables are missing from `data`: ", paste(missing_selected, collapse = ", "), call. = FALSE)
    }
  }
  structure(
    list(
      outcome = outcome,
      distribution = distribution,
      link = link,
      fixed = fixed,
      grouping = grouping,
      nesting = nesting,
      structure = structure,
      random = random,
      interactions = interactions,
      predictor_levels = predictor_levels,
      data = data
    ),
    class = "mlm_spec"
  )
}

print.mlm_spec <- function(x, ...) {
  centering <- vapply(x$fixed, function(term) term$center %||% "none", character(1))
  fixed_text <- if (length(centering)) {
    paste(ifelse(centering == "none", names(centering), sprintf("%s (%s)", names(centering), centering)), collapse = ", ")
  } else {
    "intercept only"
  }
  formula_text <- tryCatch(
    paste(deparse(build_formula(x), width.cutoff = 500), collapse = " "),
    error = function(e) "<not available>"
  )
  cat("<mlm_spec>\n")
  cat("  Outcome:      ", x$outcome, "\n", sep = "")
  cat("  Distribution: ", x$distribution, " (", x$link, " link)\n", sep = "")
  cat("  Fixed:        ", fixed_text, "\n", sep = "")
  if (length(x$interactions)) {
    cat("  Interactions: ", paste(vapply(x$interactions, paste, character(1), collapse = ":"), collapse = ", "), "\n", sep = "")
  }
  cat("  Grouping:     ", paste(unlist(x$grouping, use.names = FALSE), collapse = ", "), " (", x$structure %||% "nested", ")\n", sep = "")
  cat("  Formula:      ", formula_text, "\n", sep = "")
  if (is.data.frame(x$data)) {
    cat("  Data:         ", nrow(x$data), " rows, ", ncol(x$data), " columns\n", sep = "")
  } else {
    cat("  Data:         none attached\n")
  }
  invisible(x)
}

centering_methods <- function() c("none", "GMC", "CWC")

# Grouping variable used for cluster-mean centering of one predictor. A
# predictor can override the default (the first grouping factor) with
# `list(center = "CWC", group = "districtid")`.
centering_group <- function(fixed, term, grouping) {
  group_values <- unlist(grouping, use.names = FALSE)
  fixed[[term]]$group %||% first_or(group_values)
}

# Problems that make a specification impossible to fit as requested. Returned
# as a data frame so the same checks feed mlm_fit() errors and the app's
# readiness table.
spec_problems <- function(spec, data = spec$data) {
  check <- character()
  detail <- character()
  add <- function(item, text) {
    check <<- c(check, item)
    detail <<- c(detail, text)
  }
  group_values <- unlist(spec$grouping, use.names = FALSE)
  for (term in names(spec$fixed)) {
    method <- spec$fixed[[term]]$center %||% "none"
    if (!is.character(method) || length(method) != 1 || !method %in% centering_methods()) {
      add(paste("Centering:", term), sprintf("Centering for %s must be one of \"none\", \"GMC\", or \"CWC\".", term))
      next
    }
    if (identical(method, "none")) next
    if (is.data.frame(data) && term %in% names(data) && !is.numeric(data[[term]])) {
      add(paste("Centering:", term), sprintf("%s is not numeric, so it cannot be %s centered. Set its centering to \"none\".", term, method))
    }
    if (identical(method, "CWC")) {
      group <- centering_group(spec$fixed, term, spec$grouping)
      if (is.null(group)) {
        add(paste("Centering:", term), sprintf("Cluster-mean centering of %s requires a grouping factor.", term))
      } else if (is.data.frame(data) && !group %in% names(data)) {
        add(paste("Centering:", term), sprintf("The centering group %s for %s is missing from the data.", group, term))
      }
    }
  }
  unknown_random <- setdiff(names(spec$random), c(names(spec$grouping), group_values))
  if (length(unknown_random)) {
    add("Random effects", sprintf("Random effects were requested for %s, which is not listed in `grouping`.", paste(unknown_random, collapse = ", ")))
  }
  for (group_name in names(spec$random)) {
    unknown_slopes <- setdiff(spec$random[[group_name]]$slopes %||% character(), names(spec$fixed))
    if (length(unknown_slopes)) {
      add(
        paste("Random slope:", paste(unknown_slopes, collapse = ", ")),
        sprintf("Random slopes for %s must also be fixed predictors; add them to `fixed` or remove the random slope.", paste(unknown_slopes, collapse = ", "))
      )
    }
  }
  bad_interactions <- Filter(function(x) !is.character(x) || length(x) < 2, spec$interactions)
  if (length(bad_interactions)) {
    add("Interactions", "Each interaction must name at least two variables, such as c(\"ses\", \"sector\").")
  }
  if (!length(random_formula_terms(spec))) {
    add("Random effects", "No random effects are specified. Add a random intercept or random slope for at least one grouping factor.")
  }
  data.frame(Check = check, Detail = detail, stringsAsFactors = FALSE, check.names = FALSE)
}

# Rows that lme4 will keep: complete on every variable the model uses.
analysis_rows <- function(data, vars) {
  vars <- intersect(vars, names(data))
  if (!length(vars)) return(rep(TRUE, nrow(data)))
  stats::complete.cases(data[, vars, drop = FALSE])
}

data_ref <- function(name, data_name = "dat") {
  if (identical(make.names(name), name)) {
    paste0(data_name, "$", name)
  } else {
    sprintf("%s[[%s]]", data_name, r_string_literal(name))
  }
}

# Centers predictors using means from the analysis sample (`rows`), so a
# grand-mean-centered predictor has mean zero and a cluster-mean-centered
# predictor has cluster means of zero among the rows the model actually uses.
center_predictors <- function(data, fixed, grouping, rows = NULL) {
  centered <- data
  code_lines <- character()
  if (is.null(rows)) rows <- rep(TRUE, nrow(data))
  for (term in names(fixed)) {
    method <- fixed[[term]]$center %||% "none"
    if (!term %in% names(centered) || !is.numeric(centered[[term]]) || identical(method, "none")) {
      next
    }
    new_name <- paste0(term, "_", method)
    x <- centered[[term]]
    if (identical(method, "GMC")) {
      centered[[new_name]] <- x - mean(x[rows], na.rm = TRUE)
      code_lines <- c(code_lines, sprintf("%s <- %s - mean(%s, na.rm = TRUE)", data_ref(new_name), data_ref(term), data_ref(term)))
    } else if (identical(method, "CWC")) {
      group <- centering_group(fixed, term, grouping)
      if (is.null(group) || !group %in% names(centered)) next
      group_id <- as.character(centered[[group]])
      group_means <- tapply(x[rows], group_id[rows], function(v) mean(v, na.rm = TRUE))
      centered[[new_name]] <- x - as.numeric(group_means[group_id])
      code_lines <- c(code_lines, sprintf(
        "%s <- %s - stats::ave(%s, as.character(%s), FUN = function(x) mean(x, na.rm = TRUE))",
        data_ref(new_name), data_ref(term), data_ref(term), data_ref(group)
      ))
    }
  }
  list(data = centered, code = code_lines)
}

effective_term <- function(fixed, term) {
  if (!term %in% names(fixed)) return(term)
  method <- fixed[[term]]$center %||% "none"
  if (!identical(method, "none")) paste0(term, "_", method) else term
}

effective_fixed_terms <- function(fixed) {
  vapply(names(fixed), function(term) {
    effective_term(fixed, term)
  }, character(1))
}

effective_interaction_terms <- function(fixed, interactions) {
  vapply(interactions, function(x) paste(vapply(x, effective_term, character(1), fixed = fixed), collapse = ":"), character(1))
}

model_fixed_terms <- function(spec) {
  unique(c(effective_fixed_terms(spec$fixed), effective_interaction_terms(spec$fixed, spec$interactions)))
}

# Quote variable names that are not syntactic R names so they survive in a formula.
formula_name <- function(x) {
  ifelse(make.names(x) == x, x, paste0("`", gsub("`", "\\`", x, fixed = TRUE), "`"))
}

formula_term <- function(x) {
  vapply(strsplit(x, ":", fixed = TRUE), function(parts) paste(formula_name(parts), collapse = ":"), character(1))
}

random_formula_terms <- function(spec) {
  random_terms <- character()
  for (group_name in names(spec$random)) {
    group_var <- formula_name(spec$grouping[[group_name]] %||% group_name)
    rand <- spec$random[[group_name]]
    slopes <- rand$slopes %||% character()
    slopes <- slopes[slopes %in% names(spec$fixed)]
    slope_terms <- if (length(slopes)) formula_name(unname(effective_fixed_terms(spec$fixed[slopes]))) else character()
    include_intercept <- isTRUE(rand$intercept)
    if (!include_intercept && !length(slope_terms)) next
    if (isTRUE(rand$correlation)) {
      pieces <- c(if (include_intercept) "1" else "0", slope_terms)
      random_terms <- c(random_terms, sprintf("(%s | %s)", paste(pieces, collapse = " + "), group_var))
    } else {
      if (include_intercept) random_terms <- c(random_terms, sprintf("(1 | %s)", group_var))
      if (length(slope_terms)) {
        random_terms <- c(random_terms, sprintf("(0 + %s | %s)", slope_terms, group_var))
      }
    }
  }
  random_terms
}

build_formula <- function(spec) {
  interaction_terms <- vapply(spec$interactions, function(x) {
    paste(formula_name(vapply(x, effective_term, character(1), fixed = spec$fixed)), collapse = ":")
  }, character(1))
  rhs_fixed <- unique(c(formula_name(unname(effective_fixed_terms(spec$fixed))), interaction_terms))
  if (!length(rhs_fixed)) rhs_fixed <- "1"
  stats::as.formula(
    paste(formula_name(spec$outcome), "~", paste(c(rhs_fixed, random_formula_terms(spec)), collapse = " + ")),
    env = globalenv()
  )
}

mlm_formula <- function(spec) {
  if (!inherits(spec, "mlm_spec")) {
    stop("`spec` must be an \"mlm_spec\" object created with mlm_spec().", call. = FALSE)
  }
  build_formula(spec)
}
