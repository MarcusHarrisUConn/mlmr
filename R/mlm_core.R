example_hsb <- function(n_schools = 24, min_students = 18, max_students = 34, seed = 20260423) {
  set.seed(seed)
  n_districts <- max(4, floor(n_schools / 4))
  school_to_district <- sprintf("D%02d", rep(seq_len(n_districts), length.out = n_schools))
  school_sizes <- sample(min_students:max_students, n_schools, replace = TRUE)
  schoolid <- rep(sprintf("S%02d", seq_len(n_schools)), school_sizes)
  districtid <- school_to_district[as.integer(factor(schoolid, levels = sprintf("S%02d", seq_len(n_schools))))]
  n <- length(schoolid)
  n_teachers <- max(12, round(n_schools * 1.6))
  teacher_pool <- sprintf("T%02d", seq_len(n_teachers))
  school_teacher_map <- lapply(seq_len(n_schools), function(i) {
    sample(teacher_pool, sample(3:6, 1), replace = FALSE)
  })
  names(school_teacher_map) <- sprintf("S%02d", seq_len(n_schools))
  teacherid <- vapply(schoolid, function(s) sample(school_teacher_map[[s]], 1), character(1))
  school_sector <- sample(c("Public", "Catholic"), n_schools, replace = TRUE)
  school_meanses <- rnorm(n_schools, 0, 0.45)
  school_climate <- rnorm(n_schools, 0, 1)
  school_size <- round(rnorm(n_schools, 850, 180))
  names(school_sector) <- names(school_meanses) <- names(school_climate) <- names(school_size) <- sprintf("S%02d", seq_len(n_schools))
  u0 <- rnorm(n_schools, 0, 5.5)
  u1 <- rnorm(n_schools, 0, 1.2)
  u2 <- rnorm(n_schools, 0, 0.7)
  u3 <- rnorm(n_schools, 0, 0.5)
  d0 <- rnorm(n_districts, 0, 2.4)
  t0 <- rnorm(n_teachers, 0, 1.6)
  names(u0) <- names(u1) <- names(u2) <- names(u3) <- sprintf("S%02d", seq_len(n_schools))
  names(d0) <- sprintf("D%02d", seq_len(n_districts))
  names(t0) <- teacher_pool
  ses <- rnorm(n, school_meanses[schoolid], 0.65)
  priorachieve <- 50 + 7 * ses + rnorm(n, 0, 8)
  homework <- pmax(0, round(rnorm(n, 5 + 0.8 * ses, 1.6), 1))
  meanses <- as.numeric(school_meanses[schoolid])
  climate <- as.numeric(school_climate[schoolid])
  size <- as.numeric(school_size[schoolid])
  sector <- factor(school_sector[schoolid])
  female <- factor(rbinom(n, 1, 0.52), labels = c("Male", "Female"))
  minority <- factor(rbinom(n, 1, 0.32), labels = c("No", "Yes"))
  mathscore <- 52 +
    3.1 * ses +
    0.34 * priorachieve +
    1.05 * homework +
    2.8 * meanses +
    1.6 * climate -
    0.004 * size +
    1.9 * (sector == "Catholic") +
    1.2 * (female == "Female") -
    1.7 * (minority == "Yes") +
    1.3 * ses * (sector == "Catholic") +
    0.45 * homework * climate +
    d0[districtid] +
    t0[teacherid] +
    u0[schoolid] +
    u1[schoolid] * ses +
    u2[schoolid] * homework +
    u3[schoolid] * (priorachieve / 10) +
    rnorm(n, 0, 6.2)
  pass_prob <- stats::plogis(-0.8 + 0.055 * (mathscore - 50) + 0.45 * ses + 0.25 * (sector == "Catholic"))
  passmath <- factor(rbinom(n, 1, pass_prob), levels = c(0, 1), labels = c("No", "Yes"))
  absence_rate <- exp(1.25 - 0.22 * ses - 0.014 * priorachieve + 0.28 * (minority == "Yes"))
  absences <- rpois(n, lambda = pmax(0.05, absence_rate))
  data.frame(
    studentid = sprintf("ST%04d", seq_len(n)),
    schoolid = factor(schoolid),
    districtid = factor(districtid),
    teacherid = factor(teacherid),
    mathscore = round(mathscore, 2),
    ses = round(ses, 3),
    priorachieve = round(priorachieve, 2),
    homework = round(homework, 1),
    meanses = round(meanses, 3),
    climate = round(climate, 3),
    schoolsize = size,
    passmath = passmath,
    absences = absences,
    sector = sector,
    female = female,
    minority = minority,
    stringsAsFactors = FALSE
  )
}

numeric_vars <- function(data) {
  names(data)[vapply(data, is.numeric, logical(1))]
}

candidate_group_vars <- function(data) {
  names(data)[vapply(data, function(x) {
    is.factor(x) || is.character(x) || length(unique(x)) <= max(12, floor(nrow(data) / 8))
  }, logical(1))]
}

group_structure_table <- function(data, groups, structure = "nested") {
  groups <- groups[groups %in% names(data)]
  if (!length(groups)) {
    return(data.frame(Grouping = character(), `Number of Units` = integer(), `Rows per Unit` = character(), Structure = character(), check.names = FALSE))
  }
  do.call(rbind, lapply(groups, function(group) {
    counts <- table(data[[group]])
    data.frame(
      Grouping = group,
      `Number of Units` = length(counts),
      `Rows per Unit` = sprintf("%s-%s (M = %.1f)", min(counts), max(counts), mean(counts)),
      Structure = structure,
      check.names = FALSE
    )
  }))
}

group_structure_note <- function(groups, structure = "nested") {
  if (!length(groups)) return("Select grouping factors to describe the multilevel structure.")
  blocks <- paste(sprintf("(1 | %s)", groups), collapse = " + ")
  if (identical(structure, "crossed")) {
    return(sprintf("Crossed/non-nested structure: lme4 estimates separate random-effect blocks such as %s. This is the pattern for students taught by multiple teachers or patients connected to hospitals and doctors that do not form one strict hierarchy.", blocks))
  }
  if (identical(structure, "multiple membership")) {
    return(sprintf("Multiple-membership structure: this prototype records the design intent and generates code scaffolds, but weighted membership terms require later support beyond basic lme4 syntax. Current random-effect blocks are shown as %s.", blocks))
  }
  sprintf("Nested structure: the selected grouping factors are treated as a hierarchy or nested design note. lme4 will estimate the random-effect blocks %s.", blocks)
}

variable_role_table <- function(data, spec) {
  roles <- lapply(names(data), function(var) {
    out <- character()
    if (identical(var, spec$outcome)) out <- c(out, "outcome")
    if (var %in% names(spec$fixed)) out <- c(out, "predictor")
    if (var %in% unlist(spec$grouping, use.names = FALSE)) out <- c(out, "grouping ID")
    if (var %in% unlist(spec$interactions, use.names = FALSE)) out <- c(out, "interaction")
    center <- if (var %in% names(spec$fixed)) spec$fixed[[var]]$center %||% "none" else "none"
    if (!identical(center, "none")) out <- c(out, paste("center:", center))
    if (is.factor(data[[var]]) || is.character(data[[var]])) out <- c(out, "categorical")
    if (is.numeric(data[[var]])) out <- c(out, "numeric")
    if (!length(out)) out <- "available"
    data.frame(
      Variable = var,
      Type = if (is.numeric(data[[var]])) "numeric" else if (is.factor(data[[var]]) || is.character(data[[var]])) "categorical" else class(data[[var]])[[1]],
      Role = paste(unique(out), collapse = ", "),
      `Unique Values` = length(unique(data[[var]])),
      Missing = sum(is.na(data[[var]])),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, roles)
}

model_builder_summary <- function(spec) {
  fixed <- names(spec$fixed)
  random <- names(spec$random)
  slopes <- unique(unlist(lapply(spec$random, function(x) x$slopes %||% character())))
  data.frame(
    Component = c("Outcome", "Distribution", "Fixed predictors", "Interactions", "Grouping factors", "Random slopes", "Structure"),
    Selection = c(
      spec$outcome,
      if (identical(spec$distribution, "gaussian")) "Gaussian" else sprintf("%s (%s)", spec$distribution, spec$link),
      if (length(fixed)) paste(fixed, collapse = ", ") else "Intercept-only",
      if (length(spec$interactions)) paste(vapply(spec$interactions, paste, character(1), collapse = ":"), collapse = ", ") else "None",
      if (length(random)) paste(random, collapse = ", ") else "None",
      if (length(slopes)) paste(slopes, collapse = ", ") else "None",
      spec$structure %||% "nested"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

factor_vars <- function(data) {
  names(data)[vapply(data, function(x) is.factor(x) || is.character(x), logical(1))]
}

missingness_table <- function(data) {
  n <- nrow(data)
  data.frame(
    Variable = names(data),
    Missing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    `Percent Missing` = sprintf("%.1f%%", 100 * vapply(data, function(x) mean(is.na(x)), numeric(1))),
    check.names = FALSE,
    row.names = NULL
  )[order(vapply(data, function(x) sum(is.na(x)), integer(1)), decreasing = TRUE), ]
}

model_variables <- function(spec) {
  unique(c(spec$outcome, names(spec$fixed), unlist(spec$grouping, use.names = FALSE), unlist(spec$interactions, use.names = FALSE)))
}

model_missingness <- function(data, spec) {
  vars <- model_variables(spec)
  vars <- vars[vars %in% names(data)]
  if (!length(vars)) return(data.frame(`Rows in data` = nrow(data), `Rows complete for model` = nrow(data), `Rows with missing model data` = 0, check.names = FALSE))
  complete <- stats::complete.cases(data[, vars, drop = FALSE])
  data.frame(
    `Rows in data` = nrow(data),
    `Rows complete for model` = sum(complete),
    `Rows with missing model data` = sum(!complete),
    check.names = FALSE
  )
}

model_missingness_table <- function(data, spec) {
  vars <- model_variables(spec)
  vars <- vars[vars %in% names(data)]
  if (!length(vars)) {
    return(data.frame(Variable = character(), Role = character(), Missing = integer(), `Percent Missing` = character(), check.names = FALSE))
  }
  role_for <- function(var) {
    roles <- character()
    if (identical(var, spec$outcome)) roles <- c(roles, "outcome")
    if (var %in% names(spec$fixed)) roles <- c(roles, "fixed effect")
    if (var %in% unlist(spec$grouping, use.names = FALSE)) roles <- c(roles, "grouping factor")
    if (var %in% unlist(spec$interactions, use.names = FALSE)) roles <- c(roles, "interaction component")
    paste(unique(roles), collapse = ", ")
  }
  miss <- vapply(data[vars], function(x) sum(is.na(x)), integer(1))
  pct <- vapply(data[vars], function(x) mean(is.na(x)), numeric(1))
  data.frame(
    Variable = vars,
    Role = vapply(vars, role_for, character(1)),
    Missing = miss,
    `Percent Missing` = sprintf("%.1f%%", 100 * pct),
    check.names = FALSE,
    row.names = NULL
  )
}

model_summary_cards <- function(result, optimizer = NULL) {
  spec <- result$spec
  fit <- result$fit
  fixed <- apa_fixed_table(fit)
  fixed_non_intercept <- fixed[fixed$Predictor != "(Intercept)", , drop = FALSE]
  sig <- fixed_non_intercept[fixed_non_intercept$p == "< .001" | suppressWarnings(as.numeric(fixed_non_intercept$p)) < .05, , drop = FALSE]
  icc <- icc_table(fit)
  diag <- mlm_diagnostics(fit)
  data.frame(
    Metric = c(
      "Outcome",
      "Distribution",
      "Grouping structure",
      "Fixed effects",
      "Random-effect blocks",
      "Significant fixed effects",
      "AIC",
      "Singular fit",
      "Optimizer"
    ),
    Value = c(
      spec$outcome,
      if (identical(spec$distribution, "gaussian")) "Gaussian" else sprintf("%s (%s link)", spec$distribution, spec$link),
      spec$structure %||% "nested",
      as.character(length(names(spec$fixed))),
      paste(names(spec$random), collapse = ", "),
      if (nrow(sig)) paste(sig$Predictor, collapse = ", ") else "None at alpha = .05",
      sprintf("%.2f", stats::AIC(fit)),
      diag$value[diag$check == "Singular fit"],
      optimizer %||% "Not recorded"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

model_guidance <- function(result, REML = TRUE) {
  spec <- result$spec
  fit <- result$fit
  diag <- mlm_diagnostics(fit)
  groups <- unlist(spec$grouping, use.names = FALSE)
  warnings <- character()
  if (isTRUE(lme4::isSingular(fit, tol = 1e-4))) {
    warnings <- c(warnings, "The model is singular; at least one random-effect variance or covariance may be estimated near zero. Consider simplifying random slopes or correlations.")
  }
  messages <- diag$value[diag$check == "Convergence messages"]
  if (length(messages) && !identical(messages, "None reported")) {
    warnings <- c(warnings, paste("Convergence message:", messages))
  }
  if (identical(spec$distribution, "gaussian") && isTRUE(REML)) {
    warnings <- c(warnings, "REML is appropriate for estimating variance components, but use ML when comparing nested fixed-effect models.")
  }
  for (group in groups) {
    if (group %in% names(result$data) && length(unique(result$data[[group]])) < 10) {
      warnings <- c(warnings, sprintf("%s has fewer than 10 groups; random effects for this grouping factor may be unstable.", group))
    }
  }
  if (!length(warnings)) warnings <- "No major convergence or design warnings were detected."
  unique(warnings)
}

model_template_explanation <- function(template) {
  switch(
    template %||% "custom",
    null = "Null model: estimates unconditional clustering and ICCs before adding predictors.",
    random_intercept = "Random intercept model: allows average outcome levels to vary across groups while keeping predictor effects fixed.",
    random_slope = "Random slope model: allows selected Level-1 associations to vary across groups and estimates their variance.",
    cross_level = "Cross-level interaction model: tests whether a higher-level predictor changes a lower-level predictor slope.",
    three_level = "Three-level nested model: represents students within schools within districts, with variance partitioned across more than one higher level.",
    crossed = "Crossed model: estimates separate random-effect blocks when units are connected to more than one grouping factor, such as schools and teachers.",
    logistic = "Logistic HGLM: fits a multilevel model for a binary outcome using a binomial distribution and logit link.",
    poisson = "Poisson HGLM: fits a multilevel model for a count outcome using a Poisson distribution and log link.",
    "Custom model: uses the current selections for fixed effects, interactions, centering, and random effects."
  )
}

tau_label_table <- function(result) {
  spec <- result$spec
  fixed_terms <- model_fixed_terms(spec)
  rows <- list()
  for (group_name in names(spec$random)) {
    rand <- spec$random[[group_name]]
    random_slope_terms <- vapply(rand$slopes %||% character(), effective_term, character(1), fixed = spec$fixed)
    coefficient_labels <- c("Intercept", fixed_terms)
    random_flags <- c(isTRUE(rand$intercept), fixed_terms %in% random_slope_terms)
    block <- data.frame(
      Group = group_name,
      `Matrix Index` = paste0("u_", seq_along(coefficient_labels) - 1),
      Coefficient = coefficient_labels,
      Estimated = ifelse(random_flags, "Yes", "No; fixed only"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    rows[[group_name]] <- block
  }
  if (!length(rows)) {
    return(data.frame(Group = character(), `Matrix Index` = character(), Coefficient = character(), Estimated = character(), check.names = FALSE))
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

interpretation_points <- function(result) {
  spec <- result$spec
  fixed <- apa_fixed_table(result$fit)
  icc <- icc_table(result$fit)
  random_slopes <- unique(unlist(lapply(spec$random, function(x) x$slopes %||% character())))
  interactions <- vapply(spec$interactions, paste, character(1), collapse = " x ")
  sig <- fixed[!fixed$Predictor %in% "(Intercept)" & fixed$p != "" & fixed$p != "NA" & (fixed$p == "< .001" | suppressWarnings(as.numeric(fixed$p)) < .05), , drop = FALSE]
  c(
    sprintf("The outcome is %s, modeled with a %s multilevel specification.", spec$outcome, spec$distribution),
    sprintf("Fixed effects represent average associations after the selected centering and dummy-coding choices. %s", if (nrow(sig)) paste("Statistically detectable fixed effects include", paste(sig$Predictor, collapse = ", "), ".") else "No non-intercept fixed effects are below alpha = .05 in the current fit."),
    sprintf("Random effects are estimated for %s. Random slopes are included for %s.", paste(names(spec$random), collapse = ", "), if (length(random_slopes)) paste(random_slopes, collapse = ", ") else "no predictors"),
    sprintf("Interactions in the current model: %s.", if (length(interactions)) paste(interactions, collapse = ", ") else "none"),
    sprintf("ICCs summarize clustering after the fitted variance structure: %s.", if (nrow(icc)) paste(sprintf("%s = %.3f", icc$group, icc$icc), collapse = "; ") else "not available for this model")
  )
}

assumption_points <- function(result) {
  diag <- mlm_diagnostics(result$fit)
  c(
    "Inspect residual plots for nonlinearity, heteroskedasticity, and influential patterns before treating the fixed-effect table as final.",
    "A singular fit means one or more random-effect variance or covariance parameters are estimated near zero; simplify the random-effects structure if this persists.",
    "Use ML, not REML, when comparing nested models that differ in fixed effects.",
    "Cluster-mean centering supports within-group interpretation for Level-1 predictors; grand-mean centering supports contextual or between-group interpretation.",
    sprintf("Current diagnostics: %s.", paste(sprintf("%s = %s", diag$check, diag$value), collapse = "; "))
  )
}

model_readiness_table <- function(data, spec) {
  rows <- list()
  add <- function(status, item, detail) {
    rows[[length(rows) + 1]] <<- data.frame(
      Status = status,
      Check = item,
      Detail = detail,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  vars <- model_variables(spec)
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars)) {
    add("Stop", "Selected variables exist", paste("Missing from data:", paste(missing_vars, collapse = ", ")))
  } else {
    add("OK", "Selected variables exist", "All selected outcome, predictor, grouping, and interaction variables are present.")
  }
  if (!spec$outcome %in% names(data)) {
    add("Stop", "Outcome", "Choose an outcome variable before fitting.")
  } else if (identical(spec$distribution, "gaussian") && !is.numeric(data[[spec$outcome]])) {
    add("Stop", "Outcome type", "Gaussian models require a numeric outcome.")
  } else if (identical(spec$distribution, "binomial") && length(unique(stats::na.omit(data[[spec$outcome]]))) != 2) {
    add("Stop", "Outcome type", "Binomial models require a two-category outcome.")
  } else {
    add("OK", "Outcome type", sprintf("%s is compatible with the selected %s model.", spec$outcome, spec$distribution))
  }
  groups <- unlist(spec$grouping, use.names = FALSE)
  groups <- groups[nzchar(groups)]
  if (!length(groups)) {
    add("Stop", "Grouping factors", "Select at least one grouping factor.")
  } else {
    for (group in groups) {
      if (!group %in% names(data)) next
      n_group <- length(unique(stats::na.omit(data[[group]])))
      if (n_group < 2) {
        add("Stop", paste("Grouping:", group), "At least two groups are required for a random effect.")
      } else if (n_group < 10) {
        add("Warn", paste("Grouping:", group), sprintf("%s has %s groups; random effects may be unstable.", group, n_group))
      } else {
        add("OK", paste("Grouping:", group), sprintf("%s groups detected.", n_group))
      }
    }
  }
  random_slopes <- unique(unlist(lapply(spec$random, function(x) x$slopes %||% character())))
  for (slope in random_slopes) {
    if (!slope %in% names(data)) {
      add("Stop", paste("Random slope:", slope), "Random-slope predictor is missing from the data.")
    } else if (!is.numeric(data[[slope]])) {
      add("Stop", paste("Random slope:", slope), "Random slopes should be numeric predictors.")
    } else {
      add("OK", paste("Random slope:", slope), "Numeric predictor selected for random-slope estimation.")
    }
  }
  model_vars <- vars[vars %in% names(data)]
  if (length(model_vars)) {
    complete <- stats::complete.cases(data[, model_vars, drop = FALSE])
    pct_missing <- 100 * mean(!complete)
    if (pct_missing > 20) {
      add("Warn", "Missing model data", sprintf("%.1f%% of rows have missing values on selected model variables.", pct_missing))
    } else {
      add("OK", "Missing model data", sprintf("%.1f%% of rows have missing values on selected model variables.", pct_missing))
    }
  }
  if (!length(rows)) {
    return(data.frame(Status = "Stop", Check = "Model readiness", Detail = "No checks could be run.", check.names = FALSE))
  }
  do.call(rbind, rows)
}

model_readiness_has_stops <- function(readiness) {
  any(identical(readiness$Status, "Stop") | readiness$Status == "Stop")
}

missing_pattern_table <- function(data, spec, max_patterns = 12) {
  vars <- model_variables(spec)
  vars <- vars[vars %in% names(data)]
  if (!length(vars)) {
    return(data.frame(Pattern = character(), Rows = integer(), `Percent of Data` = character(), `Missing Variables` = character(), check.names = FALSE))
  }
  miss <- is.na(data[, vars, drop = FALSE])
  pattern <- apply(miss, 1, function(x) paste(ifelse(x, "M", "."), collapse = ""))
  tab <- sort(table(pattern), decreasing = TRUE)
  shown <- names(tab)[seq_len(min(length(tab), max_patterns))]
  out <- lapply(shown, function(pat) {
    missing_vars <- vars[strsplit(pat, "", fixed = TRUE)[[1]] == "M"]
    data.frame(
      Pattern = pat,
      Rows = as.integer(tab[[pat]]),
      `Percent of Data` = sprintf("%.1f%%", 100 * as.integer(tab[[pat]]) / nrow(data)),
      `Missing Variables` = if (length(missing_vars)) paste(missing_vars, collapse = ", ") else "Complete on model variables",
      check.names = FALSE
    )
  })
  do.call(rbind, out)
}

imputation_repro_code <- function(spec, formula = NULL, m = 20, maxit = 20, seed = 20260424) {
  vars <- model_variables(spec)
  vars <- vars[nzchar(vars)]
  formula_text <- if (is.null(formula)) "<fit formula after centering>" else paste(deparse(formula, width.cutoff = 500), collapse = " ")
  fit_line <- switch(
    spec$distribution,
    gaussian = sprintf("fit_imp <- with(imp, lme4::lmer(%s, REML = TRUE))", formula_text),
    `negative binomial` = sprintf("fit_imp <- with(imp, lme4::glmer.nb(%s))", formula_text),
    sprintf("fit_imp <- with(imp, lme4::glmer(%s, family = %s(link = \"%s\")))", formula_text, spec$distribution, spec$link)
  )
  c(
    "########################################################################",
    "# Multiple Imputation Scaffold",
    "########################################################################",
    "# Install once if needed: install.packages(c(\"mice\", \"broom.mixed\"))",
    "library(mice)",
    "library(lme4)",
    "library(broom.mixed)",
    "",
    sprintf("model_vars <- c(%s)", paste(sprintf("\"%s\"", vars), collapse = ", ")),
    "dat_model <- dat[, model_vars, drop = FALSE]",
    "",
    "# Inspect the missing-data pattern before imputing.",
    "md.pattern(dat_model)",
    "",
    "method <- make.method(dat_model)",
    "predictor_matrix <- quickpred(dat_model)",
    sprintf("imp <- mice(dat_model, m = %s, maxit = %s, method = method, predictorMatrix = predictor_matrix, seed = %s)", m, maxit, seed),
    "",
    "# Recreate any centered variables inside each completed data set before fitting.",
    "# Then fit the model across imputations and pool fixed effects.",
    fit_line,
    "pooled_fixed <- pool(fit_imp)",
    "summary(pooled_fixed, conf.int = TRUE)"
  )
}

mlm_spec <- function(outcome, distribution = "gaussian", link = "identity", fixed = list(),
                     grouping = list(), nesting = NULL, structure = "nested", random = list(),
                     interactions = list(), data = NULL) {
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
      data = data
    ),
    class = "mlm_spec"
  )
}

center_predictors <- function(data, fixed, grouping) {
  centered <- data
  code_lines <- character()
  group_values <- unlist(grouping, use.names = FALSE)
  primary_group <- if (length(group_values)) group_values[[1]] else NULL
  for (term in names(fixed)) {
    method <- fixed[[term]]$center %||% "none"
    if (!term %in% names(centered) || !is.numeric(centered[[term]]) || identical(method, "none")) {
      next
    }
    new_name <- paste0(term, "_", method)
    if (identical(method, "GMC")) {
      centered[[new_name]] <- centered[[term]] - mean(centered[[term]], na.rm = TRUE)
      code_lines <- c(code_lines, sprintf("%s <- %s - mean(%s, na.rm = TRUE)", new_name, term, term))
    } else if (identical(method, "CWC") && !is.null(primary_group) && primary_group %in% names(centered)) {
      group_mean <- ave(centered[[term]], centered[[primary_group]], FUN = function(x) mean(x, na.rm = TRUE))
      centered[[new_name]] <- centered[[term]] - group_mean
      code_lines <- c(code_lines, sprintf("%s <- %s - ave(%s, %s, FUN = function(x) mean(x, na.rm = TRUE))", new_name, term, term, primary_group))
    }
  }
  list(data = centered, code = code_lines)
}

effective_fixed_terms <- function(fixed) {
  vapply(names(fixed), function(term) {
    effective_term(fixed, term)
  }, character(1))
}

effective_term <- function(fixed, term) {
  if (!term %in% names(fixed)) return(term)
  method <- fixed[[term]]$center %||% "none"
  if (!identical(method, "none")) paste0(term, "_", method) else term
}

effective_interaction_terms <- function(fixed, interactions) {
  vapply(interactions, function(x) paste(vapply(x, effective_term, character(1), fixed = fixed), collapse = ":"), character(1))
}

model_fixed_terms <- function(spec) {
  unique(c(effective_fixed_terms(spec$fixed), effective_interaction_terms(spec$fixed, spec$interactions)))
}

build_formula <- function(spec) {
  rhs_fixed <- model_fixed_terms(spec)
  if (!length(rhs_fixed)) rhs_fixed <- "1"
  random_terms <- character()
  for (group_name in names(spec$random)) {
    group_var <- spec$grouping[[group_name]] %||% group_name
    rand <- spec$random[[group_name]]
    slopes <- rand$slopes %||% character()
    slopes <- slopes[slopes %in% names(spec$fixed)]
    slope_terms <- if (length(slopes)) effective_fixed_terms(spec$fixed[slopes]) else character()
    include_intercept <- isTRUE(rand$intercept)
    if (isTRUE(rand$correlation)) {
      pieces <- c(if (include_intercept) "1" else "0", slope_terms)
      if (length(pieces)) random_terms <- c(random_terms, sprintf("(%s | %s)", paste(pieces, collapse = " + "), group_var))
    } else {
      if (include_intercept) random_terms <- c(random_terms, sprintf("(1 | %s)", group_var))
      random_terms <- c(random_terms, sprintf("(0 + %s | %s)", slope_terms, group_var))
    }
  }
  stats::as.formula(paste(spec$outcome, "~", paste(c(rhs_fixed, random_terms), collapse = " + ")))
}

mlm_fit <- function(spec, REML = TRUE, optimizer = "bobyqa", maxfun = 20000) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required. Install it with install.packages('lme4').", call. = FALSE)
  }
  centered <- center_predictors(spec$data, spec$fixed, spec$grouping)
  spec$data <- centered$data
  formula <- build_formula(spec)
  control <- lme4::lmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))
  if (identical(spec$distribution, "gaussian")) {
    fit <- lme4::lmer(formula, data = spec$data, REML = REML, control = control)
  } else if (identical(spec$distribution, "negative binomial")) {
    glmm_control <- lme4::glmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))
    fit <- lme4::glmer.nb(formula, data = spec$data, control = glmm_control)
  } else {
    glmm_control <- lme4::glmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))
    fam <- switch(
      spec$distribution,
      binomial = stats::binomial(link = spec$link),
      poisson = stats::poisson(link = spec$link),
      Gamma = stats::Gamma(link = spec$link),
      stop("Unsupported distribution: ", spec$distribution, call. = FALSE)
    )
    fit <- lme4::glmer(formula, data = spec$data, family = fam, control = glmm_control)
  }
  list(fit = fit, formula = formula, data = spec$data, centering_code = centered$code, spec = spec)
}

variance_table <- function(fit) {
  vc <- as.data.frame(lme4::VarCorr(fit))
  vc[, intersect(c("grp", "var1", "var2", "vcov", "sdcor"), names(vc)), drop = FALSE]
}

icc_table <- function(fit) {
  vc <- as.data.frame(lme4::VarCorr(fit))
  intercepts <- vc[!identical(vc$grp, "Residual") & is.na(vc$var2) & vc$var1 == "(Intercept)", c("grp", "vcov"), drop = FALSE]
  intercepts <- intercepts[!is.na(intercepts$grp), , drop = FALSE]
  residual <- attr(lme4::VarCorr(fit), "sc")^2
  total <- sum(intercepts$vcov, na.rm = TRUE) + residual
  if (!nrow(intercepts) || !is.finite(total) || total <= 0) {
    return(data.frame(group = character(), variance = numeric(), icc = numeric()))
  }
  data.frame(
    group = intercepts$grp,
    variance = round(intercepts$vcov, 4),
    icc = round(intercepts$vcov / total, 4),
    stringsAsFactors = FALSE
  )
}

fixed_effects_table <- function(fit) {
  coefs <- coef(summary(fit))
  out <- data.frame(term = rownames(coefs), coefs, check.names = FALSE, row.names = NULL)
  names(out) <- sub("Pr\\(>\\|t\\|\\)", "p", names(out))
  stat_col <- intersect(c("t value", "z value"), names(out))
  stat <- if (length(stat_col)) out[[stat_col[[1]]]] else out$Estimate / out$`Std. Error`
  p <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
  out$p <- p
  out$CI_low <- out$Estimate - stats::qnorm(.975) * out$`Std. Error`
  out$CI_high <- out$Estimate + stats::qnorm(.975) * out$`Std. Error`
  out
}

mlm_equations <- function(result) {
  spec <- result$spec
  fixed_terms <- effective_fixed_terms(spec$fixed)
  beta_terms <- paste0("beta_", seq_along(fixed_terms), "(", fixed_terms, ")")
  fixed_rhs <- paste(c("beta_0", beta_terms), collapse = " + ")
  random_lines <- character()
  for (group_name in names(spec$random)) {
    rand <- spec$random[[group_name]]
    group_var <- spec$grouping[[group_name]] %||% group_name
    slopes <- rand$slopes %||% character()
    random_lines <- c(
      random_lines,
      sprintf("%s random effects: intercept%s vary by %s; covariance structure is %s.",
              group_name,
              if (length(slopes)) paste0(" and slopes for ", paste(slopes, collapse = ", ")) else "",
              group_var,
              if (isTRUE(rand$correlation)) "correlated" else "independent")
    )
  }
  c(
    sprintf("Fixed component: E[%s] = %s", spec$outcome, fixed_rhs),
    random_lines,
    sprintf("Fitted formula: %s", deparse(result$formula))
  )
}

latex_escape <- function(x) {
  gsub("_", "\\_", x, fixed = TRUE)
}

latex_var <- function(x) {
  paste0("\\mathrm{", latex_escape(x), "}")
}

mlm_latex_equations <- function(result) {
  spec <- result$spec
  fixed_terms <- model_fixed_terms(spec)
  fixed_labels <- latex_var(fixed_terms)
  groups <- names(spec$random)
  primary <- first_or(groups, "j")
  secondary <- if (length(groups) >= 2) groups[[2]] else NULL
  beta_rhs <- c("\\beta_{0j}", paste0("\\beta_{", seq_along(fixed_labels), "j}", fixed_labels, "_{ij}"))
  level1 <- paste0(latex_escape(spec$outcome), "_{ij} = ", paste(beta_rhs, collapse = " + "), " + r_{ij}")
  random_slope_terms <- unique(unlist(lapply(spec$random, function(x) {
    vapply(x$slopes %||% character(), effective_term, character(1), fixed = spec$fixed)
  })))
  level2 <- c("\\beta_{0j} = \\gamma_{00} + u_{0j}")
  if (length(fixed_labels)) {
    level2 <- c(level2, paste0("\\beta_{", seq_along(fixed_labels), "j} = \\gamma_{", seq_along(fixed_labels), "0}",
                               ifelse(fixed_terms %in% random_slope_terms, paste0(" + u_{", seq_along(fixed_labels), "j}"), "")))
  }
  level3 <- character()
  if (!is.null(secondary)) {
    level3 <- c("\\gamma_{00k} = \\pi_{000} + v_{00k}", "\\gamma_{p0k} = \\pi_{p00}")
  }
  combined <- paste0(
    latex_escape(spec$outcome), "_{ij} = \\gamma_{00}",
    if (length(fixed_labels)) paste0(" + ", paste(paste0("\\gamma_{", seq_along(fixed_labels), "0}", fixed_labels, "_{ij}"), collapse = " + ")) else "",
    " + u_{0j}",
    if (length(random_slope_terms)) paste0(" + ", paste(paste0("u_{", match(random_slope_terms, fixed_terms), "j}", latex_var(random_slope_terms), "_{ij}"), collapse = " + ")) else "",
    " + r_{ij}"
  )
  tau <- tau_latex(result)
  list(
    level = c("Level 1", "Level 2", if (length(level3)) "Level 3"),
    equations = c(level1, paste(level2, collapse = "\\\\ "), if (length(level3)) paste(level3, collapse = "\\\\ ")),
    combined = combined,
    tau = tau
  )
}

tau_latex <- function(result) {
  spec <- result$spec
  fixed_terms <- model_fixed_terms(spec)
  blocks <- character()
  for (group_name in names(spec$random)) {
    rand <- spec$random[[group_name]]
    random_slope_terms <- vapply(rand$slopes %||% character(), effective_term, character(1), fixed = spec$fixed)
    coefficient_labels <- c("Intercept", fixed_terms)
    random_flags <- c(isTRUE(rand$intercept), fixed_terms %in% random_slope_terms)
    n <- length(coefficient_labels)
    if (!n) next
    u_vec <- paste0("u_{", seq_len(n) - 1, "j}")
    zero_vec <- rep("0", n)
    cells <- matrix("", nrow = n, ncol = n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (j > i) {
          cells[i, j] <- "\\cdot"
        } else if (!random_flags[[i]] || !random_flags[[j]]) {
          cells[i, j] <- "0"
        } else if (i == j) {
          cells[i, j] <- sprintf("\\tau_{%s%s}", i - 1, j - 1)
        } else if (isTRUE(rand$correlation)) {
          cells[i, j] <- sprintf("\\tau_{%s%s}", i - 1, j - 1)
        } else {
          cells[i, j] <- "0"
        }
      }
    }
    row_text <- apply(cells, 1, paste, collapse = " & ")
    blocks <- c(
      blocks,
      sprintf(
        "\\begin{bmatrix}%s\\end{bmatrix}_{%s} \\sim MVN\\left(\\begin{bmatrix}%s\\end{bmatrix},\\begin{bmatrix}%s\\end{bmatrix}\\right)\\quad \\text{%s: %s}",
        paste(u_vec, collapse = "\\\\ "),
        latex_escape(group_name),
        paste(zero_vec, collapse = "\\\\ "),
        paste(row_text, collapse = "\\\\ "),
        latex_escape(group_name),
        paste(latex_escape(coefficient_labels), collapse = ", ")
      )
    )
  }
  blocks
}

apa_fixed_table <- function(fit) {
  tab <- fixed_effects_table(fit)
  stat_col <- intersect(c("t value", "z value"), names(tab))
  stat <- if (length(stat_col)) tab[[stat_col[[1]]]] else tab$Estimate / tab$`Std. Error`
  p <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
  ci_low <- tab$Estimate - stats::qnorm(.975) * tab$`Std. Error`
  ci_high <- tab$Estimate + stats::qnorm(.975) * tab$`Std. Error`
  out <- data.frame(
    Predictor = tab$term,
    b = sprintf("%.2f", tab$Estimate),
    SE = sprintf("%.2f", tab$`Std. Error`),
    Statistic = sprintf("%.2f", stat),
    p = ifelse(p < .001, "< .001", sprintf("%.3f", p)),
    `95% CI` = sprintf("[%.2f, %.2f]", ci_low, ci_high),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  out
}

latex_table_escape <- function(x) {
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("<", "$<$", x, fixed = TRUE)
  x
}

apa_fixed_latex <- function(fit, caption = "Fixed effects from the multilevel model.", label = "tab:fixed-effects") {
  tab <- apa_fixed_table(fit)
  apa_latex_table(tab, caption = caption, label = label, colspec = "lrrrrp{1.45in}")
}

apa_latex_table <- function(tab, caption, label, colspec = NULL) {
  tab[] <- lapply(tab, latex_table_escape)
  rows <- apply(tab, 1, function(x) paste(x, collapse = " & "))
  if (is.null(colspec)) {
    colspec <- paste0("l", paste(rep("r", max(0, ncol(tab) - 1)), collapse = ""))
  }
  c(
    "\\begin{table}[ht]",
    "\\centering",
    "\\small",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}", label),
    "\\resizebox{\\textwidth}{!}{%",
    sprintf("\\begin{tabular}{%s}", colspec),
    "\\hline",
    paste0(paste(latex_table_escape(names(tab)), collapse = " & "), " \\\\"),
    "\\hline",
    paste0(rows, " \\\\"),
    "\\hline",
    "\\end{tabular}",
    "}",
    "\\end{table}"
  )
}

apa_dummy_latex <- function(data, terms, caption = "Dummy coding for categorical predictors.", label = "tab:dummy-coding") {
  tab <- dummy_coding_table(data, terms)
  names(tab) <- c("Variable", "Reference Category", "Estimated Contrasts")
  apa_latex_table(tab, caption = caption, label = label, colspec = "lll")
}

apa_variance_latex <- function(fit, caption = "Random effects and variance components.", label = "tab:variance-components") {
  tab <- variance_table(fit)
  names(tab) <- c("Group", "Effect", "Covarying Effect", "Variance/Covariance", "SD/Correlation")[seq_along(names(tab))]
  numeric_cols <- vapply(tab, is.numeric, logical(1))
  tab[numeric_cols] <- lapply(tab[numeric_cols], function(x) sprintf("%.3f", x))
  tab[is.na(tab)] <- ""
  apa_latex_table(tab, caption = caption, label = label, colspec = "lllrr")
}

apa_icc_latex <- function(fit, caption = "Intraclass correlations by grouping level.", label = "tab:icc") {
  tab <- icc_table(fit)
  names(tab) <- c("Grouping Level", "Variance", "ICC")
  if (nrow(tab)) {
    tab$Variance <- sprintf("%.3f", tab$Variance)
    tab$ICC <- sprintf("%.3f", tab$ICC)
  }
  apa_latex_table(tab, caption = caption, label = label, colspec = "lrr")
}

latex_wrapped_equation <- function(equation, terms_per_line = 3) {
  equation <- paste(equation, collapse = "\\\\ ")
  pieces <- strsplit(equation, " \\+ ", fixed = FALSE)[[1]]
  if (length(pieces) <= terms_per_line + 1) return(paste0("\\[", equation, "\\]"))
  first <- pieces[[1]]
  rest <- pieces[-1]
  groups <- split(rest, ceiling(seq_along(rest) / terms_per_line))
  first_split <- strsplit(first, " = ", fixed = TRUE)[[1]]
  lhs <- first_split[[1]]
  rhs_start <- if (length(first_split) > 1) first_split[[2]] else first
  if (identical(as.integer(terms_per_line), 1L)) {
    rows <- c(
      sprintf("%s & = & %s", lhs, rhs_start),
      vapply(groups, function(g) sprintf("  &   & {} + %s", paste(g, collapse = " + ")), character(1))
    )
  } else {
    rows <- c(
      sprintf("%s & = & %s + %s", lhs, rhs_start, paste(groups[[1]], collapse = " + ")),
      vapply(groups[-1], function(g) sprintf("  &   & {} + %s", paste(g, collapse = " + ")), character(1))
    )
  }
  paste0("\\[\n\\begin{array}{rcl}\n", paste(rows, collapse = "\\\\\n"), "\n\\end{array}\n\\]")
}

latex_display_equation <- function(equation, terms_per_line = 3) {
  equation <- paste(equation, collapse = "\\\\ ")
  if (!grepl("\\\\\\\\", equation)) {
    return(latex_wrapped_equation(equation, terms_per_line = terms_per_line))
  }
  lines <- trimws(strsplit(equation, "\\\\\\\\")[[1]])
  rows <- vapply(lines[nzchar(lines)], function(line) {
    parts <- strsplit(line, " = ", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      sprintf("%s & = & %s", parts[[1]], paste(parts[-1], collapse = " = "))
    } else {
      sprintf("\\multicolumn{3}{l}{%s}", line)
    }
  }, character(1))
  paste0("\\[\n\\begin{array}{rcl}\n", paste(rows, collapse = "\\\\[0.45em]\n"), "\n\\end{array}\n\\]")
}

raw_latex_bundle <- function(result) {
  eq <- mlm_latex_equations(result)
  paste(
    "% Recommended preamble for the tables and resized Tau matrices:",
    "% \\usepackage{graphicx}",
    "",
    "% Table 1. Fixed effects",
    paste(apa_fixed_latex(result$fit), collapse = "\n"),
    "",
    "% Table 2. Dummy coding",
    paste(apa_dummy_latex(result$data, names(result$spec$fixed)), collapse = "\n"),
    "",
    "% Table 3. Variance components",
    paste(apa_variance_latex(result$fit), collapse = "\n"),
    "",
    "% Table 4. Intraclass correlations",
    paste(apa_icc_latex(result$fit), collapse = "\n"),
    "",
    "% Level-by-level equations",
    paste(vapply(eq$equations, latex_display_equation, character(1)), collapse = "\n\n"),
    "",
    "% Combined full equation",
    latex_wrapped_equation(eq$combined),
    "",
    "% Tau variance-covariance structures",
    paste(paste0("\\[\n\\resizebox{0.98\\textwidth}{!}{$\\displaystyle ", eq$tau, "$}\n\\]"), collapse = "\n\n"),
    sep = "\n"
  )
}

manuscript_report_markdown <- function(result, REML = TRUE, optimizer = "bobyqa", maxfun = 30000) {
  summary <- model_summary_cards(result, optimizer)
  diagnostics <- mlm_diagnostics(result$fit)
  fixed <- apa_fixed_table(result$fit)
  variance <- variance_table(result$fit)
  icc <- icc_table(result$fit)
  eq <- mlm_latex_equations(result)
  table_md <- function(tab) {
    if (!nrow(tab)) return("_No rows available._")
    tab[] <- lapply(tab, as.character)
    header <- paste(names(tab), collapse = " | ")
    divider <- paste(rep("---", ncol(tab)), collapse = " | ")
    rows <- apply(tab, 1, paste, collapse = " | ")
    paste(c(header, divider, rows), collapse = "\n")
  }
  c(
    "---",
    "title: \"mlmr Multilevel Model Report\"",
    "format: html",
    "---",
    "",
    "# Model Summary",
    "",
    table_md(summary),
    "",
    "# Results Write-Up",
    "",
    model_writeup(result),
    "",
    "# Fixed Effects",
    "",
    table_md(fixed),
    "",
    "# Variance Components",
    "",
    table_md(variance),
    "",
    "# Intraclass Correlations",
    "",
    table_md(icc),
    "",
    "# Equations",
    "",
    "## Level-by-Level Equations",
    "",
    paste(sprintf("$$\n%s\n$$", eq$equations), collapse = "\n\n"),
    "",
    "## Combined Full Equation",
    "",
    sprintf("$$\n%s\n$$", eq$combined),
    "",
    "## Tau Structures",
    "",
    paste(sprintf("$$\n%s\n$$", eq$tau), collapse = "\n\n"),
    "",
    "# Diagnostics",
    "",
    table_md(diagnostics),
    "",
    "# Reproducible R Code",
    "",
    "```r",
    generate_repro_code(result, REML, optimizer, maxfun),
    "```",
    "",
    "# Raw LaTeX",
    "",
    "```tex",
    raw_latex_bundle(result),
    "```"
  )
}

apa_fixed_html <- function(fit, table_number = 1, title = "Fixed Effects From the Multilevel Model") {
  tab <- apa_fixed_table(fit)
  apa_html_table(
    tab,
    table_number = table_number,
    title = title,
    note = "Confidence intervals are Wald 95% intervals. For Gaussian mixed models fit with <code>lme4</code>, p values are large-sample normal approximations and should be interpreted cautiously.",
    italic_columns = c("b", "SE", "z/t", "p")
  )
}

apa_dummy_html <- function(data, terms, table_number = 2, title = "Dummy Coding for Categorical Predictors") {
  tab <- dummy_coding_table(data, terms)
  names(tab) <- c("Variable", "Reference Category", "Estimated Contrasts")
  apa_html_table(
    tab,
    table_number = table_number,
    title = title,
    note = "Reference categories are the omitted categories against which displayed contrasts are interpreted."
  )
}

apa_variance_html <- function(fit, table_number = 3, title = "Random Effects and Variance Components") {
  tab <- variance_table(fit)
  names(tab) <- c("Group", "Effect", "Covarying Effect", "Variance/Covariance", "SD/Correlation")[seq_along(names(tab))]
  numeric_cols <- vapply(tab, is.numeric, logical(1))
  tab[numeric_cols] <- lapply(tab[numeric_cols], function(x) sprintf("%.3f", x))
  tab[is.na(tab)] <- ""
  apa_html_table(
    tab,
    table_number = table_number,
    title = title,
    note = "Rows with two effects describe covariance or correlation parameters; single-effect rows describe variances and standard deviations."
  )
}

apa_icc_html <- function(fit, table_number = 4, title = "Intraclass Correlations by Grouping Level") {
  tab <- icc_table(fit)
  names(tab) <- c("Grouping Level", "Variance", "ICC")
  if (nrow(tab)) {
    tab$Variance <- sprintf("%.3f", tab$Variance)
    tab$ICC <- sprintf("%.3f", tab$ICC)
  }
  apa_html_table(
    tab,
    table_number = table_number,
    title = title,
    note = "ICCs represent the proportion of model-implied variance attributable to each grouping level."
  )
}

apa_tables_html_document <- function(result) {
  paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>mlmr APA Tables</title>",
    "<style>",
    "body{font-family:'Times New Roman',serif;max-width:980px;margin:32px auto;line-height:1.35;color:#111;}",
    ".apa-table-wrap{margin-bottom:28px;}.apa-table-number{font-weight:bold;margin-bottom:4px;}",
    ".apa-table-title{font-style:italic;margin-bottom:8px;}.apa-table{width:100%;border-collapse:collapse;border-top:1.5px solid currentColor;border-bottom:1.5px solid currentColor;}",
    ".apa-table thead{border-bottom:1px solid currentColor;}.apa-table th,.apa-table td{padding:6px 8px;border:none;vertical-align:top;text-align:right;}",
    ".apa-table th:first-child,.apa-table td:first-child{text-align:left;}.apa-table th{font-weight:normal;}.apa-table-note{margin-top:8px;}",
    "</style></head><body>",
    apa_fixed_html(result$fit),
    apa_dummy_html(result$data, names(result$spec$fixed)),
    apa_variance_html(result$fit),
    apa_icc_html(result$fit),
    "</body></html>"
  )
}

apa_tables_latex_document <- function(result) {
  paste(
    "% mlmr APA manuscript tables",
    "% Recommended preamble: \\usepackage{graphicx}",
    paste(apa_fixed_latex(result$fit), collapse = "\n"),
    "",
    paste(apa_dummy_latex(result$data, names(result$spec$fixed)), collapse = "\n"),
    "",
    paste(apa_variance_latex(result$fit), collapse = "\n"),
    "",
    paste(apa_icc_latex(result$fit), collapse = "\n"),
    sep = "\n"
  )
}

apa_html_table <- function(tab, table_number, title, note = NULL, italic_columns = character()) {
  if (!nrow(tab)) {
    return(paste0(
      "<div class='apa-table-wrap'>",
      sprintf("<div class='apa-table-number'>Table %s</div>", table_number),
      sprintf("<div class='apa-table-title'>%s</div>", html_escape(title)),
      "<div class='apa-empty'>No rows are available for this table.</div>",
      "</div>"
    ))
  }
  header <- paste(vapply(names(tab), function(x) {
    label <- if (x %in% italic_columns) sprintf("<em>%s</em>", html_escape(x)) else html_escape(x)
    sprintf("<th>%s</th>", label)
  }, character(1)), collapse = "")
  rows <- apply(tab, 1, function(x) {
    sprintf(
      "<tr>%s</tr>",
      paste(vapply(x, function(cell) sprintf("<td>%s</td>", html_escape(cell)), character(1)), collapse = "")
    )
  })
  paste0(
    "<div class='apa-table-wrap'>",
    sprintf("<div class='apa-table-number'>Table %s</div>", table_number),
    sprintf("<div class='apa-table-title'>%s</div>", html_escape(title)),
    "<table class='apa-table'>",
    sprintf("<thead><tr>%s</tr></thead>", header),
    "<tbody>",
    paste(rows, collapse = ""),
    "</tbody></table>",
    if (!is.null(note) && nzchar(note)) sprintf("<div class='apa-table-note'><em>Note.</em> %s</div>", note) else "",
    "</div>"
  )
}

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

model_writeup <- function(result) {
  fit <- result$fit
  spec <- result$spec
  fixed <- apa_fixed_table(fit)
  icc <- icc_table(fit)
  diag <- mlm_diagnostics(fit)
  formula_text <- paste(deparse(result$formula, width.cutoff = 500), collapse = " ")
  outcome <- spec$outcome
  groups <- names(spec$random)
  random_slopes <- unique(unlist(lapply(spec$random, function(x) x$slopes %||% character())))
  sig <- fixed[!fixed$Predictor %in% "(Intercept)" & fixed$p != "" & fixed$p != "NA" & (fixed$p == "< .001" | suppressWarnings(as.numeric(fixed$p)) < .05), , drop = FALSE]
  sig_text <- if (nrow(sig)) {
    paste(sprintf("%s (b = %s, SE = %s, p = %s, 95%% CI %s)", sig$Predictor, sig$b, sig$SE, sig$p, sig$`95% CI`), collapse = "; ")
  } else {
    "No fixed effects met the conventional alpha = .05 threshold in the fitted model."
  }
  icc_text <- if (nrow(icc)) {
    paste(sprintf("%s ICC = %.3f", icc$group, icc$icc), collapse = "; ")
  } else {
    "ICC values were not available for this model."
  }
  paste(
    sprintf("A multilevel linear model was estimated for %s using restricted maximum likelihood unless ML was selected in the estimation panel. The fitted model was specified as: %s.", outcome, formula_text),
    sprintf("The model included fixed effects for %s and random effects for %s. Random slope variation was estimated for %s, allowing the corresponding Level-1 associations to vary across higher-level units.", paste(names(spec$fixed), collapse = ", "), paste(groups, collapse = ", "), if (length(random_slopes)) paste(random_slopes, collapse = ", ") else "no predictors"),
    sprintf("The fixed-effect results indicated the following statistically detectable associations: %s.", sig_text),
    sprintf("Variance partitioning suggested the following clustering structure: %s. These ICC values quantify the proportion of model-implied residual variance attributable to each grouping level, conditional on the random-effects structure.", icc_text),
    sprintf("Model diagnostics reported: %s. Singularity or convergence warnings should be interpreted as evidence that the specified random-effects structure may be too complex for the available data or may require alternative centering, simplification, or optimizer settings.", paste(sprintf("%s = %s", diag$check, diag$value), collapse = "; ")),
    "Substantively, the model separates within-unit predictors from higher-level contextual predictors while estimating heterogeneity in selected slopes. This is the central advantage of the multilevel specification over a single-level regression: it permits the average association, the between-group variance, and the group-to-group variation in key effects to be evaluated within one coherent model.",
    sep = "\n\n"
  )
}

dummy_coding_table <- function(data, terms) {
  terms <- intersect(terms, factor_vars(data))
  if (!length(terms)) {
    return(data.frame(variable = character(), reference = character(), contrasts = character()))
  }
  do.call(rbind, lapply(terms, function(term) {
    x <- factor(data[[term]])
    levels_x <- levels(x)
    data.frame(
      variable = term,
      reference = first_or(levels_x, ""),
      contrasts = paste(setdiff(levels_x, first_or(levels_x, "")), collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
}

mlm_diagnostics <- function(fit) {
  messages <- fit@optinfo$conv$lme4$messages
  gradient <- fit@optinfo$derivs$gradient
  overdispersion <- if (inherits(fit, "glmerMod")) {
    ratio <- sum(stats::resid(fit, type = "pearson")^2, na.rm = TRUE) / stats::df.residual(fit)
    format(ratio, digits = 4)
  } else {
    "Not applicable"
  }
  data.frame(
    check = c("Singular fit", "Convergence messages", "Maximum absolute gradient", "Overdispersion ratio"),
    value = c(
      as.character(lme4::isSingular(fit, tol = 1e-4)),
      if (is.null(messages)) "None reported" else paste(messages, collapse = "; "),
      if (is.null(gradient)) "Not available" else format(max(abs(gradient)), digits = 4),
      overdispersion
    ),
    stringsAsFactors = FALSE
  )
}

fit_code_lines <- function(spec, formula_text, REML, optimizer, maxfun) {
  if (identical(spec$distribution, "gaussian")) {
    return(c(
      sprintf("fit <- lmer(%s, data = dat, REML = %s,", formula_text, if (isTRUE(REML)) "TRUE" else "FALSE"),
      sprintf("            control = lmerControl(optimizer = \"%s\", optCtrl = list(maxfun = %s)))", optimizer, maxfun)
    ))
  }
  control_line <- sprintf("glmerControl(optimizer = \"%s\", optCtrl = list(maxfun = %s))", optimizer, maxfun)
  if (identical(spec$distribution, "negative binomial")) {
    return(c(
      sprintf("fit <- glmer.nb(%s, data = dat,", formula_text),
      sprintf("                control = %s)", control_line)
    ))
  }
  family_call <- switch(
    spec$distribution,
    binomial = sprintf("binomial(link = \"%s\")", spec$link),
    poisson = sprintf("poisson(link = \"%s\")", spec$link),
    Gamma = sprintf("Gamma(link = \"%s\")", spec$link),
    sprintf("%s(link = \"%s\")", spec$distribution, spec$link)
  )
  c(
    sprintf("fit <- glmer(%s, data = dat, family = %s,", formula_text, family_call),
    sprintf("             control = %s)", control_line)
  )
}

generate_repro_code <- function(result, REML, optimizer, maxfun) {
  spec <- result$spec
  latex <- mlm_latex_equations(result)
  formula_text <- paste(deparse(result$formula, width.cutoff = 500), collapse = " ")
  factor_terms <- intersect(names(spec$fixed), factor_vars(result$data))
  section <- function(title) c(strrep("#", 72), paste("#", title), strrep("#", 72))
  c(
    section("Packages"),
    "library(lme4)",
    "",
    section("Model Structure"),
    sprintf("# Grouping structure selected in the app: %s", spec$structure %||% "nested"),
    sprintf("# Grouping factors: %s", paste(unlist(spec$grouping, use.names = FALSE), collapse = ", ")),
    sprintf("# Structure note: %s", spec$nesting %||% ""),
    "",
    section("Center Predictors"),
    "# Center predictors as requested in the app.",
    result$centering_code,
    "",
    section("Fit Multilevel Model"),
    fit_code_lines(spec, formula_text, REML, optimizer, maxfun),
    "",
    "summary(fit)",
    "",
    section("Table Helpers"),
    "# Helper functions for manuscript-style tables",
    "fmt_p <- function(p) ifelse(p < .001, \"< .001\", sprintf(\"%.3f\", p))",
    "latex_escape <- function(x) {",
    "  x <- gsub(\"\\\\\\\\\", \"\\\\\\\\textbackslash{}\", x, fixed = TRUE)",
    "  x <- gsub(\"_\", \"\\\\\\\\_\", x, fixed = TRUE)",
    "  x <- gsub(\"%\", \"\\\\\\\\%\", x, fixed = TRUE)",
    "  x <- gsub(\"&\", \"\\\\\\\\&\", x, fixed = TRUE)",
    "  x <- gsub(\"<\", \"$<$\", x, fixed = TRUE)",
    "  x",
    "}",
    "",
    section("Table 1: APA Fixed Effects"),
    "# Table 1. APA-style fixed effects",
    "coefs <- coef(summary(fit))",
    "stat_col <- grep(\" value$\", colnames(coefs), value = TRUE)[1]",
    "stat <- coefs[, stat_col]",
    "p <- 2 * pnorm(abs(stat), lower.tail = FALSE)",
    "apa_table <- data.frame(",
    "  Predictor = rownames(coefs),",
    "  b = sprintf(\"%.2f\", coefs[, \"Estimate\"]),",
    "  SE = sprintf(\"%.2f\", coefs[, \"Std. Error\"]),",
    "  Statistic = sprintf(\"%.2f\", stat),",
    "  p = fmt_p(p),",
    "  `95% CI` = sprintf(\"[%.2f, %.2f]\", coefs[, \"Estimate\"] - qnorm(.975) * coefs[, \"Std. Error\"], coefs[, \"Estimate\"] + qnorm(.975) * coefs[, \"Std. Error\"]),",
    "  check.names = FALSE",
    ")",
    "apa_table",
    "",
    section("Table 2: Dummy Coding"),
    "# Table 2. Dummy coding for categorical predictors",
    sprintf("factor_terms <- c(%s)", paste(sprintf("\"%s\"", factor_terms), collapse = ", ")),
    "dummy_table <- do.call(rbind, lapply(factor_terms, function(term) {",
    "  x <- factor(dat[[term]])",
    "  data.frame(",
    "    Variable = term,",
    "    `Reference Category` = levels(x)[1],",
    "    `Estimated Contrasts` = paste(levels(x)[-1], collapse = \", \"),",
    "    check.names = FALSE",
    "  )",
    "}))",
    "dummy_table",
    "",
    section("Table 3: Variance Components"),
    "# Table 3. Random effects and variance components",
    "variance_table <- as.data.frame(VarCorr(fit))",
    "variance_table <- variance_table[, intersect(c(\"grp\", \"var1\", \"var2\", \"vcov\", \"sdcor\"), names(variance_table))]",
    "names(variance_table) <- c(\"Group\", \"Effect\", \"Covarying Effect\", \"Variance/Covariance\", \"SD/Correlation\")[seq_along(names(variance_table))]",
    "variance_table",
    "",
    section("Table 4: Intraclass Correlations"),
    "# Table 4. Intraclass correlations",
    "vc <- as.data.frame(VarCorr(fit))",
    "intercepts <- vc[!is.na(vc$grp) & is.na(vc$var2) & vc$var1 == \"(Intercept)\", c(\"grp\", \"vcov\"), drop = FALSE]",
    "residual_var <- attr(VarCorr(fit), \"sc\")^2",
    "total_var <- sum(intercepts$vcov, na.rm = TRUE) + residual_var",
    "icc_table <- data.frame(",
    "  `Grouping Level` = intercepts$grp,",
    "  Variance = round(intercepts$vcov, 3),",
    "  ICC = round(intercepts$vcov / total_var, 3),",
    "  check.names = FALSE",
    ")",
    "icc_table",
    "",
    section("APA LaTeX Table"),
    "# Manuscript-ready LaTeX table",
    "fixed_effects_latex <- paste(c(",
    paste0("  \"", gsub("\\", "\\\\", apa_fixed_latex(result$fit), fixed = TRUE), "\"", collapse = ",\n"),
    "), collapse = \"\\n\")",
    "cat(fixed_effects_latex)",
    "",
    "# Raw LaTeX for all manuscript tables is available in the app's",
    "# Copy/Paste Raw LaTeX panel.",
    "",
    section("Empirical Bayes Estimates"),
    "# Empirical Bayes estimates",
    "ranef(fit)",
    "",
    section("Raw LaTeX Equations"),
    "# Raw LaTeX equations generated by mlmr",
    "level_equations <- c(",
    paste0("  \"", gsub("\\", "\\\\", latex$equations, fixed = TRUE), "\"", collapse = ",\n"),
    ")",
    "combined_equation <- ",
    paste0("  \"", gsub("\\", "\\\\", latex$combined, fixed = TRUE), "\""),
    "tau_matrices <- c(",
    paste0("  \"", gsub("\\", "\\\\", latex$tau, fixed = TRUE), "\"", collapse = ",\n"),
    ")",
    "",
    imputation_repro_code(spec, result$formula)
  )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

first_or <- function(x, y = NULL) {
  if (is.null(x) || !length(x)) y else x[[1]]
}
