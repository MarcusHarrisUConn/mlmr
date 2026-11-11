example_hsb <- function(n_schools = 24, min_students = 18, max_students = 34, seed = 20260423) {
  if (!is.numeric(n_schools) || length(n_schools) != 1 || is.na(n_schools) || n_schools < 2) {
    stop("`n_schools` must be a single number greater than or equal to 2.", call. = FALSE)
  }
  if (!is.numeric(min_students) || !is.numeric(max_students) ||
      length(min_students) != 1 || length(max_students) != 1 ||
      is.na(min_students) || is.na(max_students) ||
      min_students < 1 || max_students < min_students) {
    stop("`min_students` and `max_students` must define a valid positive range.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
    stop("`seed` must be a single numeric value.", call. = FALSE)
  }
  n_schools <- as.integer(n_schools)
  min_students <- as.integer(min_students)
  max_students <- as.integer(max_students)
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

predictor_level_table <- function(data, spec) {
  fixed <- names(spec$fixed)
  if (!length(fixed)) {
    return(data.frame(
      Predictor = character(),
      `Model Role` = character(),
      `Declared Level` = character(),
      Centering = character(),
      `Within-Group Variation` = character(),
      check.names = FALSE
    ))
  }

  groups <- unlist(spec$grouping, use.names = FALSE)
  primary_group <- first_or(groups)
  declared_levels <- spec$predictor_levels %||% list()
  declared_level <- function(var) {
    hit <- names(Filter(function(x) var %in% x, declared_levels))
    if (length(hit)) hit[[1]] else "Not assigned"
  }
  within_text <- function(var) {
    if (!var %in% names(data) || is.null(primary_group) || !primary_group %in% names(data)) return("Not checked")
    within_counts <- stats::aggregate(
      data[[var]],
      list(group = data[[primary_group]]),
      function(x) length(unique(x[!is.na(x)]))
    )$x
    if (all(within_counts <= 1, na.rm = TRUE)) {
      sprintf("Constant within %s", primary_group)
    } else {
      sprintf("Varies within %s", primary_group)
    }
  }

  interaction_components <- unique(unlist(spec$interactions, use.names = FALSE))
  data.frame(
    Predictor = fixed,
    `Model Role` = ifelse(fixed %in% interaction_components, "Fixed effect + interaction component", "Fixed effect"),
    `Declared Level` = vapply(fixed, declared_level, character(1)),
    Centering = vapply(fixed, function(x) spec$fixed[[x]]$center %||% "none", character(1)),
    `Within-Group Variation` = vapply(fixed, within_text, character(1)),
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

fixed_effects_note <- function(fit) {
  if (is_gaussian_fit(fit)) {
    "For Gaussian mixed models fit with lme4, p values use large-sample normal approximations; use them as screening evidence unless a denominator-df method is added."
  } else {
    "For generalized mixed models, p values and intervals are Wald large-sample approximations on the link scale."
  }
}

icc_note <- function(fit) {
  if (is_gaussian_fit(fit)) {
    "ICCs represent the proportion of model-implied variance attributable to each grouping level for this Gaussian mixed model."
  } else {
    "Classical Gaussian ICCs are not computed for generalized mixed models; use model-family-specific latent- or observation-scale ICC methods before reporting ICCs."
  }
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

model_guidance <- function(result, REML = NULL) {
  REML <- result_settings(result, REML = REML)$REML
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
  if (!identical(spec$distribution, "gaussian")) {
    warnings <- c(warnings, "This is a generalized mixed model workflow. Review link-scale interpretation, sparse cells or overdispersion, and family-specific reporting before publication.")
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
  rows <- lapply(tau_blocks(result), function(block) {
    data.frame(
      Group = block$group,
      `Matrix Index` = equation_plain(block$symbols),
      Coefficient = block$labels,
      Estimated = "Yes",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  if (!length(rows)) {
    return(data.frame(Group = character(), `Matrix Index` = character(), Coefficient = character(), Estimated = character(), check.names = FALSE))
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

equation_key_table <- function(result) {
  key <- equation_structure(result)$key
  data.frame(
    Symbol = key$Symbol,
    Coefficient = key$Coefficient,
    Estimate = sprintf("%.3f", key$Estimate),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
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
    sprintf("Fixed effects represent average associations after the selected centering and dummy-coding choices. %s %s", if (nrow(sig)) paste("Statistically detectable fixed effects include", paste(sig$Predictor, collapse = ", "), ".") else "No non-intercept fixed effects are below alpha = .05 in the current fit.", fixed_effects_note(result$fit)),
    sprintf("Random effects are estimated for %s. Random slopes are included for %s.", paste(names(spec$random), collapse = ", "), if (length(random_slopes)) paste(random_slopes, collapse = ", ") else "no predictors"),
    sprintf("Interactions in the current model: %s.", if (length(interactions)) paste(interactions, collapse = ", ") else "none"),
    sprintf("%s %s", icc_note(result$fit), if (nrow(icc)) paste("Current ICC estimates:", paste(sprintf("%s = %.3f", icc$group, icc$icc), collapse = "; ")) else "")
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
  } else if (identical(spec$distribution, "poisson") && (!is.numeric(data[[spec$outcome]]) || any(data[[spec$outcome]] < 0, na.rm = TRUE) || any(abs(data[[spec$outcome]] - round(data[[spec$outcome]])) > .Machine$double.eps^0.5, na.rm = TRUE))) {
    add("Stop", "Outcome type", "Poisson models require a nonnegative integer count outcome.")
  } else if (identical(spec$distribution, "negative binomial") && (!is.numeric(data[[spec$outcome]]) || any(data[[spec$outcome]] < 0, na.rm = TRUE) || any(abs(data[[spec$outcome]] - round(data[[spec$outcome]])) > .Machine$double.eps^0.5, na.rm = TRUE))) {
    add("Stop", "Outcome type", "Negative-binomial models require a nonnegative integer count outcome.")
  } else if (identical(spec$distribution, "Gamma") && (!is.numeric(data[[spec$outcome]]) || any(data[[spec$outcome]] <= 0, na.rm = TRUE))) {
    add("Stop", "Outcome type", "Gamma models require a strictly positive numeric outcome.")
  } else {
    add("OK", "Outcome type", sprintf("%s is compatible with the selected %s model.", spec$outcome, spec$distribution))
  }
  if (!family_link_supported(spec$distribution, spec$link)) {
    add("Stop", "Distribution/link pair", family_link_detail(spec$distribution))
  } else {
    add("OK", "Distribution/link pair", family_link_detail(spec$distribution))
  }
  problems <- spec_problems(spec, data)
  for (i in seq_len(nrow(problems))) {
    add("Stop", problems$Check[[i]], problems$Detail[[i]])
  }
  groups <- unlist(spec$grouping, use.names = FALSE)
  groups <- groups[nzchar(groups)]
  if (!length(groups)) {
    add("Stop", "Grouping factors", "Select at least one grouping factor.")
  } else {
    nesting <- nesting_check(data, groups, spec$structure %||% "nested")
    if (!is.null(nesting)) add(nesting$status, "Nesting of grouping IDs", nesting$detail)
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
      slope_groups <- intersect(groups, names(data))
      for (group in slope_groups) {
        counts <- stats::aggregate(data[[slope]], list(group = data[[group]]), function(x) length(unique(x[!is.na(x)])))$x
        invariant <- sum(counts <= 1, na.rm = TRUE)
        if (invariant == length(counts)) {
          add("Stop", paste("Random slope variation:", slope), sprintf("%s does not vary within any %s unit; a random slope is not identified for this grouping factor.", slope, group))
        } else if (invariant > 0) {
          add("Warn", paste("Random slope variation:", slope), sprintf("%s is constant within %s of %s %s units; inspect whether the random slope is supported by the design.", slope, invariant, length(counts), group))
        } else {
          add("OK", paste("Random slope variation:", slope), sprintf("%s varies within all observed %s units.", slope, group))
        }
      }
    }
  }
  if (identical(spec$distribution, "binomial") && spec$outcome %in% names(data)) {
    outcome_values <- stats::na.omit(data[[spec$outcome]])
    for (group in groups[groups %in% names(data)]) {
      group_outcomes <- stats::aggregate(outcome_values, list(group = data[[group]][!is.na(data[[spec$outcome]])]), function(x) length(unique(x)))
      one_class <- sum(group_outcomes$x <= 1, na.rm = TRUE)
      if (one_class > 0) {
        add("Warn", paste("Binomial separation:", group), sprintf("%s of %s %s units contain only one outcome category; check separation and sparse cells before reporting.", one_class, nrow(group_outcomes), group))
      } else {
        add("OK", paste("Binomial separation:", group), sprintf("Both outcome categories appear within all observed %s units.", group))
      }
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
  any(readiness$Status == "Stop")
}

# lme4 treats `(1 | school) + (1 | district)` as nested only when lower-level
# IDs are unique across higher-level units. Reused IDs (school "1" in every
# district) silently turn a nested design into a crossed one.
nesting_check <- function(data, groups, structure = "nested") {
  groups <- intersect(groups, names(data))
  if (length(groups) < 2) return(NULL)
  spanning_share <- function(inner, outer) {
    keep <- !is.na(data[[inner]]) & !is.na(data[[outer]])
    if (!any(keep)) return(0)
    memberships <- tapply(as.character(data[[outer]][keep]), as.character(data[[inner]][keep]), function(x) length(unique(x)))
    mean(memberships > 1)
  }
  pairs <- utils::combn(groups, 2, simplify = FALSE)
  not_nested <- character()
  hierarchy <- character()
  for (pair in pairs) {
    share_a <- spanning_share(pair[[1]], pair[[2]])
    share_b <- spanning_share(pair[[2]], pair[[1]])
    if (share_a > 0 && share_b > 0) {
      not_nested <- c(not_nested, sprintf(
        "%s and %s (%.0f%% of %s units span several %s units; %.0f%% of %s units span several %s units)",
        pair[[1]], pair[[2]], 100 * share_a, pair[[1]], pair[[2]], 100 * share_b, pair[[2]], pair[[1]]
      ))
    } else {
      hierarchy <- c(hierarchy, if (share_a == 0) sprintf("%s within %s", pair[[1]], pair[[2]]) else sprintf("%s within %s", pair[[2]], pair[[1]]))
    }
  }
  if (identical(structure, "nested")) {
    if (length(not_nested)) {
      return(list(status = "Warn", detail = sprintf(
        "These grouping factors are not nested as coded: %s. lme4 will treat them as crossed. If the design is nested and lower-level IDs restart within each higher-level unit, create IDs that are unique across higher-level units (for example with interaction()); otherwise select the crossed structure.",
        paste(not_nested, collapse = "; ")
      )))
    }
    return(list(status = "OK", detail = sprintf("Grouping IDs form a strict hierarchy (%s).", paste(hierarchy, collapse = "; "))))
  }
  if (identical(structure, "crossed") && !length(not_nested)) {
    return(list(status = "Warn", detail = sprintf("The grouping factors form a strict hierarchy in these data (%s); the design may be nested rather than crossed.", paste(hierarchy, collapse = "; "))))
  }
  NULL
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

imputation_repro_code <- function(spec, formula = NULL, m = 20, maxit = 20, seed = 20260424, data_name = "dat") {
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
    sprintf("dat_model <- %s[, model_vars, drop = FALSE]", data_name),
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
    selected <- unique(c(outcome, names(fixed), unlist(grouping, use.names = FALSE), unlist(interactions, use.names = FALSE)))
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
    sprintf("%s[[\"%s\"]]", data_name, gsub("\"", "\\\"", name, fixed = TRUE))
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
        "%s <- %s - ave(%s, %s, FUN = function(x) mean(x, na.rm = TRUE))",
        data_ref(new_name), data_ref(term), data_ref(term), data_ref(group)
      ))
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
      random_terms <- c(random_terms, sprintf("(0 + %s | %s)", slope_terms, group_var))
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

mlm_fit <- function(spec, REML = TRUE, optimizer = "bobyqa", maxfun = 20000) {
  if (!inherits(spec, "mlm_spec")) {
    stop("`spec` must be an \"mlm_spec\" object created with mlm_spec().", call. = FALSE)
  }
  if (!is.data.frame(spec$data)) {
    stop("The specification has no data. Supply `data` to mlm_spec() before fitting.", call. = FALSE)
  }
  if (!family_link_supported(spec$distribution, spec$link)) {
    stop("Unsupported distribution/link pair. ", family_link_detail(spec$distribution), call. = FALSE)
  }
  missing_vars <- setdiff(model_variables(spec), names(spec$data))
  if (length(missing_vars)) {
    stop("The following selected variables are missing from `data`: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }
  problems <- spec_problems(spec)
  if (nrow(problems)) {
    stop("The model specification cannot be fit:\n", paste0("  - ", problems$Detail, collapse = "\n"), call. = FALSE)
  }
  rows <- analysis_rows(spec$data, model_variables(spec))
  if (!any(rows)) {
    stop("No rows are complete on the selected model variables.", call. = FALSE)
  }
  centered <- center_predictors(spec$data, spec$fixed, spec$grouping, rows = rows)
  spec$data <- centered$data
  formula <- build_formula(spec)
  if (identical(spec$distribution, "gaussian")) {
    control <- lme4::lmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))
    fit <- suppressMessages(lme4::lmer(formula, data = spec$data, REML = REML, control = control))
  } else if (identical(spec$distribution, "negative binomial")) {
    glmm_control <- lme4::glmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))
    fit <- suppressMessages(lme4::glmer.nb(formula, data = spec$data, control = glmm_control))
  } else {
    glmm_control <- lme4::glmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))
    fam <- switch(
      spec$distribution,
      binomial = stats::binomial(link = spec$link),
      poisson = stats::poisson(link = spec$link),
      Gamma = stats::Gamma(link = spec$link),
      stop("Unsupported distribution: ", spec$distribution, call. = FALSE)
    )
    fit <- suppressMessages(lme4::glmer(formula, data = spec$data, family = fam, control = glmm_control))
  }
  structure(
    list(
      fit = fit,
      formula = formula,
      data = spec$data,
      centering_code = centered$code,
      spec = spec,
      settings = list(
        REML = identical(spec$distribution, "gaussian") && isTRUE(REML),
        optimizer = optimizer,
        maxfun = maxfun
      )
    ),
    class = "mlm_result"
  )
}

print.mlm_result <- function(x, ...) {
  fit <- x$fit
  estimator <- if (is_gaussian_fit(fit)) {
    if (lme4::isREML(fit)) "REML" else "ML"
  } else {
    "ML (Laplace approximation)"
  }
  group_sizes <- lme4::ngrps(fit)
  cat("<mlm_result>\n")
  cat("  Formula:      ", paste(deparse(x$formula, width.cutoff = 500), collapse = " "), "\n", sep = "")
  cat("  Distribution: ", x$spec$distribution, " (", x$spec$link, " link), estimated by ", estimator, "\n", sep = "")
  cat("  Observations: ", stats::nobs(fit), " of ", nrow(x$data), " rows\n", sep = "")
  cat("  Groups:       ", paste(sprintf("%s = %s", names(group_sizes), group_sizes), collapse = ", "), "\n", sep = "")
  cat("  AIC:          ", sprintf("%.2f", stats::AIC(fit)), "\n", sep = "")
  cat("  Singular fit: ", if (isTRUE(lme4::isSingular(fit, tol = 1e-4))) "yes" else "no", "\n\n", sep = "")
  print(apa_fixed_table(fit), row.names = FALSE)
  cat("\nUse mlm_apa_tables(), mlm_latex_equations(), or mlm_publication_bundle() for reporting output.\n")
  invisible(x)
}

# Estimation settings for reporting. The fitted model is the source of truth:
# values recorded by mlm_fit() win over arguments, which are only used for
# results that do not carry settings (for example hand-built result lists).
result_settings <- function(result, REML = NULL, optimizer = NULL, maxfun = NULL) {
  stored <- result$settings %||% list()
  fit_reml <- tryCatch(is_gaussian_fit(result$fit) && lme4::isREML(result$fit), error = function(e) NULL)
  list(
    REML = isTRUE(fit_reml %||% stored$REML %||% REML %||% TRUE),
    optimizer = stored$optimizer %||% optimizer %||% "bobyqa",
    maxfun = stored$maxfun %||% maxfun %||% 20000
  )
}

variance_table <- function(fit) {
  vc <- as.data.frame(lme4::VarCorr(fit))
  vc[, intersect(c("grp", "var1", "var2", "vcov", "sdcor"), names(vc)), drop = FALSE]
}

icc_table <- function(fit) {
  if (!is_gaussian_fit(fit)) {
    return(data.frame(group = character(), variance = numeric(), icc = numeric()))
  }
  vc <- as.data.frame(lme4::VarCorr(fit))
  is_intercept <- vc$grp != "Residual" & is.na(vc$var2) & !is.na(vc$var1) & vc$var1 == "(Intercept)"
  intercepts <- vc[is_intercept, c("grp", "vcov"), drop = FALSE]
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

mlm_default_packages <- function() {
  c("R", "mlmr", "lme4", "shiny", "bslib", "ggplot2")
}

mlm_supported_models <- function() {
  data.frame(
    Area = c(
      "Model families",
      "Nested structures",
      "Three-level structures",
      "Crossed random effects",
      "Multiple membership",
      "Repeated measures",
      "Random effects",
      "Centering",
      "Interactions",
      "Categorical predictors",
      "Uploaded data",
      "Diagnostics",
      "Reporting exports",
      "Software citations"
    ),
    Status = c(
      "Supported with scope warnings",
      "Supported",
      "Supported",
      "Experimental",
      "Planned",
      "Supported through grouping structure",
      "Supported",
      "Supported",
      "Supported",
      "Supported through R contrasts",
      "Supported",
      "Supported",
      "Supported",
      "Supported"
    ),
    Scope = c(
      "Gaussian linear mixed models are the primary production path; binomial, Poisson, negative binomial, and Gamma GLMMs are available as advanced workflows.",
      "Two-level nested models are supported in the app and backend.",
      "Three-level nested models are supported through additional grouping factors and random-effect blocks.",
      "Separate lme4 random-effect blocks can be specified; users should verify the design and interpretation carefully.",
      "Weighted membership models are outside the current lme4-backed production scope.",
      "Longitudinal models can be specified when occasions, persons, and higher-level units are represented by grouping variables.",
      "Random intercepts, random slopes, and correlated or independent random-effect structures are supported.",
      "No centering, grand-mean centering, and cluster-mean centering are supported for numeric predictors.",
      "Fixed interactions and cross-level interaction-style terms are supported.",
      "Factors and character variables are modeled using R's contrast system; dummy-coding summaries are reported.",
      "CSV, TSV/TXT, Excel, SPSS, SAS, and Stata files are supported when optional readers are installed.",
      "Convergence messages, singular-fit checks, gradients, overdispersion for GLMMs, and variance summaries are reported; users should still evaluate design-specific diagnostics.",
      "APA tables, raw LaTeX, equations, Quarto-ready reports, and reproducible R code are supported with explicit Wald-inference and GLMM ICC cautions.",
      "R/package version tables, APA software statements, and optional papaja citation code are supported."
    ),
    User_responsibility = c(
      "Confirm distributional assumptions and link functions.",
      "Confirm the grouping IDs correctly represent the design.",
      "Confirm enough units exist at each higher level for stable estimation.",
      "Confirm the design is truly crossed and not a miscoded hierarchy.",
      "Use specialized methods outside mlmr for weighted multiple-membership models.",
      "Confirm time is coded appropriately and that autocorrelation assumptions are acceptable.",
      "Inspect convergence, singular fits, and whether the random-effects structure is supported by the data.",
      "Match centering choices to the research question.",
      "Interpret lower-order effects conditionally when interactions are included.",
      "Confirm reference categories and contrasts before reporting.",
      "Check variable types, missing data, and imported labels before fitting.",
      "Treat diagnostics as evidence for revision, not as automatic pass/fail decisions.",
      "Review all manuscript text, labels, notation, and inferential method choices before submission.",
      "Confirm package citations meet the target journal or style guide."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

mlm_hlm8_parity <- function() {
  data.frame(
    HLM8_workflow = c(
      "Unconditional/null model",
      "Random-intercept model",
      "Random-slope model",
      "Cross-level interaction",
      "Two-level nested model",
      "Three-level nested model",
      "Centering decisions",
      "Binary outcome HGLM",
      "Count outcome HGLM",
      "Model comparison",
      "Variance components and ICCs",
      "Equation display",
      "Diagnostics",
      "Publication tables",
      "Reproducible syntax export",
      "Software citation and methods text",
      "Missing-data review",
      "Multiple-imputation pooling",
      "Reliability summaries",
      "Fully weighted multiple membership"
    ),
    mlmr_status = c(
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Experimental",
      "Experimental",
      "Supported",
      "Supported for Gaussian LMMs",
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Planned",
      "Planned",
      "Out of current scope"
    ),
    Evidence = c(
      "Model template and formula generation support intercept-only models.",
      "Model template and lme4 random-intercept syntax are available.",
      "Random slopes can be selected for numeric fixed predictors.",
      "Fixed interactions are supported, including lower-by-higher level terms.",
      "Grouping factors and random-effect blocks support standard two-level models.",
      "Additional grouping factors support three-level nested models.",
      "No centering, grand-mean centering, and cluster-mean centering are available.",
      "Advanced mode fits binomial mixed models through lme4::glmer().",
      "Advanced mode fits Poisson and negative-binomial mixed models.",
      "Stored model history reports AIC, BIC, log likelihood, and LRT output.",
      "Variance components are generated after fitting; classical ICC tables are generated for Gaussian mixed models and suppressed with warnings for GLMMs.",
      "Level-by-level, combined, and Tau matrix equations are generated.",
      "Convergence, singular fit, gradients, overdispersion, residual plots, and fitted plots are available.",
      "APA fixed effects, dummy coding, variance components, Gaussian ICC, and software tables are available with inference notes.",
      "Downloadable R code and Quarto-ready reports are generated from fitted models.",
      "Publication bundle includes methods text, reproducibility checklist, and software statements.",
      "Missingness summaries and imputation scaffold code are available.",
      "Scaffold code exists, but pooled mixed-model reporting is not yet implemented.",
      "Reliability-style HLM summaries need explicit design and validation.",
      "Weighted multiple-membership models require methods outside the current lme4-backed production path."
    ),
    Next_step = c(
      "Add benchmark comparison against a published null-model example.",
      "Add benchmark estimates for common classroom examples.",
      "Add more guardrails for over-complex random-slope structures.",
      "Add interaction plotting and simple-slopes export.",
      "Add a dedicated two-level tutorial and validation fixture.",
      "Add a dedicated three-level tutorial and validation fixture.",
      "Add teaching popovers explaining within, between, and contextual effects.",
      "Add stronger binary-outcome diagnostics and reporting language.",
      "Add count-specific diagnostics and incidence-rate-ratio table option.",
      "Add clearer ML/REML switching guidance in the GUI.",
      "Add reliability-style summaries where statistically appropriate.",
      "Add notation customization for manuscript labels.",
      "Add influence and cluster-level diagnostic summaries.",
      "Add journal-style table presets.",
      "Add one-click reproducibility bundle archive.",
      "Add a software-paper-ready citation and methods appendix vignette.",
      "Add explicit missing-data decision checklist.",
      "Implement and validate pooling workflow before advertising as supported.",
      "Define formulas and compare against HLM documentation/examples.",
      "Document alternatives and avoid presenting as HLM parity."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

benchmark_specs <- function(dat) {
  list(
    null = mlm_spec(
      outcome = "mathscore",
      fixed = list(),
      grouping = list(schoolid = "schoolid"),
      random = list(schoolid = list(intercept = TRUE, slopes = character(), correlation = TRUE)),
      data = dat
    ),
    random_intercept = mlm_spec(
      outcome = "mathscore",
      fixed = list(ses = list(center = "CWC"), meanses = list(center = "GMC")),
      grouping = list(schoolid = "schoolid"),
      random = list(schoolid = list(intercept = TRUE, slopes = character(), correlation = TRUE)),
      data = dat
    ),
    random_slope = mlm_spec(
      outcome = "mathscore",
      fixed = list(ses = list(center = "CWC"), meanses = list(center = "GMC")),
      grouping = list(schoolid = "schoolid"),
      random = list(schoolid = list(intercept = TRUE, slopes = "ses", correlation = TRUE)),
      data = dat
    ),
    cross_level = mlm_spec(
      outcome = "mathscore",
      fixed = list(ses = list(center = "CWC"), sector = list(center = "none")),
      grouping = list(schoolid = "schoolid"),
      random = list(schoolid = list(intercept = TRUE, slopes = "ses", correlation = TRUE)),
      interactions = list(c("ses", "sector")),
      data = dat
    ),
    three_level = mlm_spec(
      outcome = "mathscore",
      fixed = list(ses = list(center = "CWC"), meanses = list(center = "GMC")),
      grouping = list(schoolid = "schoolid", districtid = "districtid"),
      random = list(
        schoolid = list(intercept = TRUE, slopes = "ses", correlation = TRUE),
        districtid = list(intercept = TRUE, slopes = character(), correlation = TRUE)
      ),
      data = dat
    )
  )
}

mlm_validation_benchmarks <- function(run = FALSE, seed = 20260423, tolerance = 0.5) {
  dat <- example_hsb(n_schools = 8, min_students = 8, max_students = 10, seed = seed)
  expected <- data.frame(
    Benchmark = c("null", "random_intercept", "random_slope", "cross_level", "three_level"),
    `Expected Formula` = c(
      "mathscore ~ 1 + (1 | schoolid)",
      "mathscore ~ ses_CWC + meanses_GMC + (1 | schoolid)",
      "mathscore ~ ses_CWC + meanses_GMC + (1 + ses_CWC | schoolid)",
      "mathscore ~ ses_CWC + sector + ses_CWC:sector + (1 + ses_CWC | schoolid)",
      "mathscore ~ ses_CWC + meanses_GMC + (1 + ses_CWC | schoolid) + (1 | districtid)"
    ),
    `Expected AIC` = c(566.8003, 537.3551, 540.7158, 539.3033, 542.7158),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  specs <- benchmark_specs(dat)
  rows <- lapply(names(specs), function(name) {
    spec <- specs[[name]]
    formula_text <- paste(deparse(build_formula(spec), width.cutoff = 500), collapse = " ")
    readiness <- model_readiness_table(dat, spec)
    expected_row <- expected[expected$Benchmark == name, , drop = FALSE]
    row <- data.frame(
      Benchmark = name,
      Formula = formula_text,
      `Expected Formula` = expected_row$`Expected Formula`,
      `Formula Match` = identical(formula_text, expected_row$`Expected Formula`),
      `Readiness Stops` = sum(readiness$Status == "Stop"),
      `Fit Tested` = "No",
      AIC = NA_real_,
      `Expected AIC` = expected_row$`Expected AIC`,
      `AIC Difference` = NA_real_,
      `Validation Status` = if (identical(formula_text, expected_row$`Expected Formula`) && sum(readiness$Status == "Stop") == 0) "Formula validated" else "Review",
      `Fixed Effects` = length(names(spec$fixed)),
      `Random Blocks` = length(names(spec$random)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (isTRUE(run)) {
      fit <- mlm_fit(spec, REML = TRUE, optimizer = "bobyqa", maxfun = 10000)
      row$`Fit Tested` <- "Yes"
      row$AIC <- stats::AIC(fit$fit)
      row$`AIC Difference` <- abs(row$AIC - row$`Expected AIC`)
      row$`Validation Status` <- if (
        isTRUE(row$`Formula Match`) &&
          row$`Readiness Stops` == 0 &&
          is.finite(row$`AIC Difference`) &&
          row$`AIC Difference` <= tolerance
      ) "Validated" else "Review"
    }
    row
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

mlm_validation_report <- function(seed = 20260423, tolerance = 0.5) {
  dat <- example_hsb(n_schools = 8, min_students = 8, max_students = 10, seed = seed)
  specs <- benchmark_specs(dat)
  add_benchmark <- function(tab, name) {
    if (!nrow(tab)) return(tab)
    data.frame(Benchmark = name, tab, check.names = FALSE, stringsAsFactors = FALSE)
  }
  fits <- lapply(specs, function(spec) {
    mlm_fit(spec, REML = TRUE, optimizer = "bobyqa", maxfun = 10000)
  })
  fixed <- do.call(rbind, lapply(names(fits), function(name) add_benchmark(apa_fixed_table(fits[[name]]$fit), name)))
  variance <- do.call(rbind, lapply(names(fits), function(name) add_benchmark(variance_table(fits[[name]]$fit), name)))
  icc <- do.call(rbind, lapply(names(fits), function(name) add_benchmark(icc_table(fits[[name]]$fit), name)))
  diagnostics <- do.call(rbind, lapply(names(fits), function(name) add_benchmark(mlm_diagnostics(fits[[name]]$fit), name)))
  row.names(fixed) <- row.names(variance) <- row.names(icc) <- row.names(diagnostics) <- NULL
  list(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    seed = seed,
    tolerance = tolerance,
    benchmarks = mlm_validation_benchmarks(run = TRUE, seed = seed, tolerance = tolerance),
    fixed_effects = fixed,
    variance_components = variance,
    icc = icc,
    diagnostics = diagnostics
  )
}

package_version_safe <- function(package) {
  if (identical(package, "R")) {
    return(paste(R.version$major, R.version$minor, sep = "."))
  }
  version <- tryCatch(
    as.character(utils::packageVersion(package)),
    error = function(e) NA_character_
  )
  if (is.na(version) && identical(package, "mlmr") && file.exists("DESCRIPTION")) {
    desc <- tryCatch(read.dcf("DESCRIPTION"), error = function(e) NULL)
    if (!is.null(desc) && "Version" %in% colnames(desc)) {
      version <- as.character(desc[1, "Version"])
    }
  }
  version
}

package_purpose <- function(package) {
  switch(
    package,
    R = "Statistical computing environment",
    mlmr = "Guided mixed-effects and multilevel modeling workflow",
    lme4 = "Mixed-effects model estimation",
    shiny = "Interactive web application framework",
    bslib = "User interface theming",
    ggplot2 = "Diagnostic and reporting graphics",
    haven = "SPSS, SAS, and Stata file import",
    readxl = "Excel file import",
    rmarkdown = "Report rendering",
    knitr = "Dynamic report generation",
    "Analysis dependency"
  )
}

package_citation_label <- function(package) {
  switch(
    package,
    R = "R Core Team",
    mlmr = "Harris",
    lme4 = "Bates et al.",
    shiny = "Chang et al.",
    bslib = "Sievert and Cheng",
    ggplot2 = "Wickham",
    haven = "Wickham et al.",
    readxl = "Wickham and Bryan",
    rmarkdown = "Allaire et al.",
    knitr = "Xie",
    package
  )
}

mlm_software_table <- function(packages = mlm_default_packages()) {
  packages <- unique(packages[nzchar(packages)])
  data.frame(
    Software = packages,
    Version = vapply(packages, package_version_safe, character(1)),
    Purpose = vapply(packages, package_purpose, character(1)),
    Citation = vapply(packages, package_citation_label, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

mlm_software_apa <- function(packages = mlm_default_packages()) {
  tab <- mlm_software_table(packages)
  r_version <- tab$Version[tab$Software == "R"]
  pkg_tab <- tab[tab$Software != "R", , drop = FALSE]
  package_text <- if (nrow(pkg_tab)) {
    paste(sprintf("%s version %s", pkg_tab$Software, pkg_tab$Version), collapse = ", ")
  } else {
    "no additional R packages"
  }
  sprintf(
    "Analyses were conducted in R version %s. Model estimation, reporting, and reproducible exports used %s. Package versions are reported to support computational reproducibility.",
    if (length(r_version) && !is.na(r_version[[1]])) r_version[[1]] else paste(R.version$major, R.version$minor, sep = "."),
    package_text
  )
}

mlm_papaja_code <- function(packages = mlm_default_packages(), bib_file = "r-references.bib") {
  packages <- unique(packages[nzchar(packages)])
  package_text <- paste(sprintf("\"%s\"", packages), collapse = ", ")
  c(
    "# Optional papaja workflow for APA-style software citations.",
    "# Install once if needed: install.packages(\"papaja\")",
    sprintf("reported_packages <- c(%s)", package_text),
    "",
    "if (requireNamespace(\"papaja\", quietly = TRUE)) {",
    sprintf("  papaja::r_refs(file = \"%s\")", bib_file),
    "  software_citation <- papaja::cite_r(",
    sprintf("    file = \"%s\",", bib_file),
    "    pkgs = reported_packages,",
    "    omit = FALSE",
    "  )",
    "  software_citation",
    "} else {",
    "  message(\"Install papaja to generate APA-style R/package citations and BibTeX references.\")",
    "}"
  )
}

apa_software_latex <- function(packages = mlm_default_packages(), caption = "Software and R packages used in the analysis.", label = "tab:software") {
  tab <- mlm_software_table(packages)
  apa_latex_table(tab, caption = caption, label = label)
}

apa_software_html <- function(packages = mlm_default_packages(), table_number = 5, title = "Software and R Packages Used in the Analysis") {
  apa_html_table(
    mlm_software_table(packages),
    table_number = table_number,
    title = title,
    note = "Package versions should be reported with manuscripts or supplementary materials so analyses can be reproduced in the same computational environment."
  )
}

software_repro_code <- function(packages = mlm_default_packages()) {
  packages <- unique(packages[nzchar(packages)])
  c(
    "reported_packages <- c(",
    paste0("  \"", packages, "\"", collapse = ",\n"),
    ")",
    "package_versions <- data.frame(",
    "  Software = reported_packages,",
    "  Version = vapply(reported_packages, function(pkg) {",
    "    if (identical(pkg, \"R\")) {",
    "      paste(R.version$major, R.version$minor, sep = \".\")",
    "    } else {",
    "      as.character(utils::packageVersion(pkg))",
    "    }",
    "  }, character(1)),",
    "  check.names = FALSE",
    ")",
    "package_versions",
    "",
    "software_statement <- sprintf(",
    "  \"Analyses were conducted in R version %s using %s.\",",
    "  package_versions$Version[package_versions$Software == \"R\"],",
    "  paste(sprintf(\"%s version %s\", package_versions$Software[package_versions$Software != \"R\"], package_versions$Version[package_versions$Software != \"R\"]), collapse = \", \")",
    ")",
    "software_statement",
    "",
    mlm_papaja_code(packages)
  )
}

fixed_effects_table <- function(fit) {
  coefs <- coef(summary(fit))
  stat_col <- intersect(c("t value", "z value"), colnames(coefs))
  estimate <- coefs[, "Estimate"]
  se <- coefs[, "Std. Error"]
  stat <- if (length(stat_col)) coefs[, stat_col[[1]]] else estimate / se
  out <- data.frame(term = rownames(coefs), coefs, check.names = FALSE, row.names = NULL)
  out$p <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
  out$CI_low <- estimate - stats::qnorm(.975) * se
  out$CI_high <- estimate + stats::qnorm(.975) * se
  out
}

# Label for exponentiated coefficients, or NULL when exponentiating is not
# meaningful for the model's link function.
exponentiated_label <- function(fit) {
  if (is_gaussian_fit(fit)) return(NULL)
  fam <- stats::family(fit)
  if (identical(fam$link, "logit")) return("OR")
  if (identical(fam$link, "log")) {
    return(if (grepl("^(poisson|Negative Binomial)", fam$family)) "IRR" else "exp(b)")
  }
  NULL
}

mlm_equations <- function(result) {
  spec <- result$spec
  coef_names <- names(lme4::fixef(result$fit))
  slopes <- setdiff(coef_names, "(Intercept)")
  beta_terms <- sprintf("b%s*%s", seq_along(slopes), slopes)
  fixed_rhs <- paste(c(if ("(Intercept)" %in% coef_names) "b0", beta_terms), collapse = " + ")
  lhs <- if (is_gaussian_fit(result$fit)) {
    sprintf("E[%s]", spec$outcome)
  } else {
    sprintf("%s(E[%s])", stats::family(result$fit)$link, spec$outcome)
  }
  cnms <- lme4::getME(result$fit, "cnms")
  random_lines <- vapply(unique(names(cnms)), function(group) {
    components <- cnms[names(cnms) == group]
    labels <- unlist(components, use.names = FALSE)
    structure_text <- if (length(labels) < 2) {
      "a single variance"
    } else if (length(components) == 1) {
      "correlated effects"
    } else if (all(lengths(components) == 1)) {
      "independent effects"
    } else {
      "partly correlated effects"
    }
    sprintf(
      "Random effects by %s: %s (%s).",
      group, paste(sub("(Intercept)", "intercept", labels, fixed = TRUE), collapse = ", "), structure_text
    )
  }, character(1))
  c(
    sprintf("Fixed component: %s = %s", lhs, fixed_rhs),
    unname(random_lines),
    sprintf("Fitted formula: %s", paste(deparse(result$formula, width.cutoff = 500), collapse = " "))
  )
}

latex_escape <- function(x) {
  x <- gsub("\\", "/", x, fixed = TRUE)
  x <- gsub("([{}$#%&^~`])", "", x, perl = TRUE)
  gsub("_", "\\_", x, fixed = TRUE)
}

latex_var <- function(x) {
  paste0("\\mathrm{", latex_escape(x), "}")
}

# Subscript indices are concatenated HLM-style (gamma_10) unless an index
# needs two digits, in which case they are comma separated.
equation_index <- function(index) {
  if (any(index > 9)) paste(index, collapse = ",") else paste(index, collapse = "")
}

constant_within <- function(x, group) {
  x <- as.character(x)
  first <- x[match(group, group)]
  same <- x == first
  same[is.na(x) & is.na(first)] <- TRUE
  same[is.na(same)] <- FALSE
  all(same)
}

is_nested_in <- function(lower, higher) {
  constant_within(higher, lower)
}

# Orders grouping factors from the lowest level (most units) upward and reports
# whether they form a strict hierarchy in the fitted data.
equation_groups <- function(fit) {
  flist <- lme4::getME(fit, "flist")
  groups <- unique(names(lme4::getME(fit, "cnms")))
  n_units <- vapply(groups, function(g) nlevels(droplevels(flist[[g]])), integer(1))
  groups <- groups[order(-n_units)]
  nested <- TRUE
  if (length(groups) > 1) {
    for (i in seq_len(length(groups) - 1)) {
      if (!is_nested_in(flist[[groups[[i]]]], flist[[groups[[i + 1]]]])) nested <- FALSE
    }
  }
  list(groups = groups, nested = nested, flist = flist)
}

# Splits every fixed-effect coefficient into the parts that vary at each level
# of a nested design. Returns NULL when the model cannot be written in
# level-by-level form (crossed factors, four or more levels, or coefficient
# names that cannot be matched to model terms).
equation_levels <- function(fit) {
  grp <- equation_groups(fit)
  groups <- grp$groups
  if (!grp$nested || length(groups) > 2) return(NULL)
  n_levels <- length(groups) + 1L
  X <- lme4::getME(fit, "X")
  coef_names <- colnames(X)
  assign <- attr(X, "assign")
  term_labels <- attr(stats::terms(fit), "term.labels")
  if (is.null(assign) || length(assign) != length(coef_names)) return(NULL)
  frame <- stats::model.frame(fit)
  variable_level <- function(variable) {
    variable <- gsub("`", "", variable, fixed = TRUE)
    if (!variable %in% names(frame)) return(NA_integer_)
    level <- 1L
    for (group in groups) {
      if (!constant_within(frame[[variable]], grp$flist[[group]])) break
      level <- level + 1L
    }
    level
  }
  parts <- matrix("", nrow = length(coef_names), ncol = n_levels)
  for (j in seq_along(coef_names)) {
    if (assign[[j]] == 0) next
    variables <- strsplit(term_labels[[assign[[j]]]], ":", fixed = TRUE)[[1]]
    components <- strsplit(coef_names[[j]], ":", fixed = TRUE)[[1]]
    if (length(variables) != length(components)) return(NULL)
    levels <- vapply(variables, variable_level, integer(1))
    if (anyNA(levels)) return(NULL)
    for (level in seq_len(n_levels)) {
      parts[j, level] <- paste(components[levels == level], collapse = ":")
    }
  }
  # Index each coefficient within its parent: an empty part is index 0.
  index <- matrix(0L, nrow = length(coef_names), ncol = n_levels)
  for (level in seq_len(n_levels)) {
    parent <- if (level == 1) rep("", length(coef_names)) else apply(index[, seq_len(level - 1), drop = FALSE], 1, paste, collapse = ".")
    for (key in unique(parent)) {
      rows <- which(parent == key)
      labels <- unique(parts[rows, level])
      labels <- labels[nzchar(labels)]
      index[rows, level] <- match(parts[rows, level], labels, nomatch = 0L)
    }
  }
  # Map each random effect to the coefficient equation it belongs to.
  cnms <- lme4::getME(fit, "cnms")
  random <- list()
  for (depth in seq_along(groups)) {
    components <- cnms[names(cnms) == groups[[depth]]]
    labels <- unlist(components, use.names = FALSE)
    prefixes <- matrix(0L, nrow = length(labels), ncol = depth)
    for (i in seq_along(labels)) {
      if (identical(labels[[i]], "(Intercept)")) next
      row <- match(labels[[i]], coef_names)
      if (is.na(row) || any(index[row, seq_len(n_levels) > depth] != 0L)) return(NULL)
      prefixes[i, ] <- index[row, seq_len(depth)]
    }
    random[[groups[[depth]]]] <- list(
      labels = labels,
      prefixes = prefixes,
      component = rep(seq_along(components), lengths(components))
    )
  }
  order_rows <- do.call(order, as.data.frame(index))
  list(
    n_levels = n_levels, groups = groups, coef_names = coef_names,
    parts = parts, index = index, random = random, order = order_rows
  )
}

equation_link_line <- function(result, eta, outcome_tex) {
  link <- stats::family(result$fit)$link
  mean_tex <- if (identical(result$spec$distribution, "binomial")) {
    sprintf("P(%s = 1)", outcome_tex)
  } else {
    sprintf("E(%s)", outcome_tex)
  }
  rhs <- switch(
    link,
    logit = sprintf("\\mathrm{logit}\\left[%s\\right]", mean_tex),
    probit = sprintf("\\mathrm{probit}\\left[%s\\right]", mean_tex),
    log = sprintf("\\mathrm{log}\\left[%s\\right]", mean_tex),
    inverse = sprintf("1 / %s", mean_tex),
    sprintf("\\mathrm{%s}\\left[%s\\right]", latex_escape(link), mean_tex)
  )
  paste(eta, "=", rhs)
}

tau_block_latex <- function(symbols, cells, group, labels) {
  sprintf(
    "\\begin{bmatrix}%s\\end{bmatrix}_{%s} \\sim MVN\\left(\\begin{bmatrix}%s\\end{bmatrix},\\begin{bmatrix}%s\\end{bmatrix}\\right)\\quad \\text{%s: %s}",
    paste(symbols, collapse = "\\\\ "),
    latex_escape(group),
    paste(rep("0", length(symbols)), collapse = "\\\\ "),
    paste(apply(cells, 1, paste, collapse = " & "), collapse = "\\\\ "),
    latex_escape(group),
    paste(latex_escape(labels), collapse = ", ")
  )
}

equation_plain <- function(x) {
  x <- gsub("\\\\(beta|gamma|pi|tau|eta)", "\\1", x)
  x <- gsub("[{}]", "", x)
  gsub("\\", "", x, fixed = TRUE)
}

# Level-by-level (hierarchical) notation for two- and three-level nested models.
equations_hierarchical <- function(result, lev) {
  fit <- result$fit
  n_levels <- lev$n_levels
  gaussian <- is_gaussian_fit(fit)
  coef_symbol <- if (n_levels == 2) c("\\beta", "\\gamma") else c("\\pi", "\\beta", "\\gamma")
  random_symbol <- if (n_levels == 2) "u" else c("r", "u")
  residual_symbol <- if (n_levels == 2) "r" else "e"
  subs <- if (n_levels == 2) c("ij", "j") else c("ijk", "jk", "k")
  tails <- substring(subs, 2)
  index <- lev$index
  parts <- lev$parts
  symbol <- function(base, prefix, tail) sprintf("%s_{%s%s}", base, equation_index(prefix), tail)
  variable_tex <- function(part, sub) {
    if (!nzchar(part)) return("")
    components <- strsplit(part, ":", fixed = TRUE)[[1]]
    paste(sprintf("%s_{%s}", latex_var(components), sub), collapse = "\\,")
  }
  key_of <- function(prefix) paste(prefix, collapse = ".")
  part_for <- function(prefix) {
    depth <- length(prefix)
    rows <- which(apply(index[, seq_len(depth), drop = FALSE], 1, key_of) == key_of(prefix))
    parts[rows[[1]], depth]
  }
  has_random <- function(depth, prefix) {
    rand <- lev$random[[lev$groups[[depth]]]]
    any(apply(rand$prefixes, 1, key_of) == key_of(prefix))
  }
  outcome_tex <- sprintf("%s_{%s}", latex_var(result$spec$outcome), subs[[1]])
  eta <- sprintf("\\eta_{%s}", subs[[1]])
  lhs <- if (gaussian) outcome_tex else eta

  # Level 1.
  level1_children <- sort(unique(index[, 1]))
  level1_terms <- vapply(level1_children, function(p) {
    paste0(symbol(coef_symbol[[1]], p, tails[[1]]), variable_tex(part_for(p), subs[[1]]))
  }, character(1))
  level1 <- paste(lhs, "=", paste(c(level1_terms, if (gaussian) sprintf("%s_{%s}", residual_symbol, subs[[1]])), collapse = " + "))
  if (!gaussian) level1 <- paste(equation_link_line(result, eta, outcome_tex), level1, sep = "\\\\ ")

  # Higher levels: one equation per coefficient of the level below.
  higher <- lapply(seq_len(n_levels - 1), function(depth) {
    prefixes <- unique(index[lev$order, seq_len(depth), drop = FALSE])
    lines <- vapply(seq_len(nrow(prefixes)), function(i) {
      prefix <- prefixes[i, ]
      rows <- which(apply(index[, seq_len(depth), drop = FALSE], 1, key_of) == key_of(prefix))
      children <- sort(unique(index[rows, depth + 1]))
      terms <- vapply(children, function(child) {
        paste0(
          symbol(coef_symbol[[depth + 1]], c(prefix, child), tails[[depth + 1]]),
          variable_tex(part_for(c(prefix, child)), subs[[depth + 1]])
        )
      }, character(1))
      if (has_random(depth, prefix)) terms <- c(terms, symbol(random_symbol[[depth]], prefix, tails[[depth]]))
      paste(symbol(coef_symbol[[depth]], prefix, tails[[depth]]), "=", paste(terms, collapse = " + "))
    }, character(1))
    paste(lines, collapse = "\\\\ ")
  })

  # Combined (mixed-model) equation.
  fixed_terms <- vapply(lev$order, function(row) {
    variables <- vapply(rev(seq_len(n_levels)), function(level) variable_tex(parts[row, level], subs[[level]]), character(1))
    paste(c(symbol("\\gamma", index[row, ], ""), variables[nzchar(variables)]), collapse = "")
  }, character(1))
  random_terms <- unlist(lapply(seq_along(lev$groups), function(depth) {
    rand <- lev$random[[lev$groups[[depth]]]]
    prefixes <- unique(rand$prefixes[do.call(order, as.data.frame(rand$prefixes)), , drop = FALSE])
    vapply(seq_len(nrow(prefixes)), function(i) {
      prefix <- prefixes[i, ]
      variables <- vapply(rev(seq_len(depth)), function(level) {
        if (prefix[[level]] == 0L) "" else variable_tex(part_for(prefix[seq_len(level)]), subs[[level]])
      }, character(1))
      paste(c(symbol(random_symbol[[depth]], prefix, tails[[depth]]), variables[nzchar(variables)]), collapse = "")
    }, character(1))
  }))
  combined <- paste(lhs, "=", paste(c(fixed_terms, random_terms, if (gaussian) sprintf("%s_{%s}", residual_symbol, subs[[1]])), collapse = " + "))

  # Tau matrices, one per grouping factor.
  tau_tag <- if (n_levels == 2) "" else c("\\pi ", "\\beta ")
  blocks <- lapply(seq_along(lev$groups), function(depth) {
    group <- lev$groups[[depth]]
    rand <- lev$random[[group]]
    labels_index <- apply(rand$prefixes, 1, equation_index)
    symbols <- vapply(seq_along(rand$labels), function(i) symbol(random_symbol[[depth]], rand$prefixes[i, ], tails[[depth]]), character(1))
    n <- length(symbols)
    cells <- matrix("0", nrow = n, ncol = n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (rand$component[[i]] != rand$component[[j]]) next
        pair <- c(labels_index[[max(i, j)]], labels_index[[min(i, j)]])
        cells[i, j] <- sprintf("\\tau_{%s%s}", tau_tag[[depth]], paste(pair, collapse = if (depth > 1 || any(nchar(pair) > 1)) "," else ""))
      }
    }
    list(group = group, labels = rand$labels, symbols = symbols, cells = cells)
  })

  key <- data.frame(
    Symbol = equation_plain(vapply(lev$order, function(row) symbol("\\gamma", index[row, ], ""), character(1))),
    Coefficient = lev$coef_names[lev$order],
    Estimate = unname(lme4::fixef(fit)[lev$coef_names[lev$order]]),
    stringsAsFactors = FALSE
  )
  level_labels <- c("Level 1", sprintf("Level %s (%s)", seq_along(lev$groups) + 1L, lev$groups))
  list(
    level = level_labels,
    equations = c(level1, unlist(higher)),
    combined = combined,
    blocks = blocks,
    key = key,
    notation = if (n_levels == 2) "two-level hierarchical" else "three-level hierarchical"
  )
}

# Single mixed-model equation for crossed or otherwise non-hierarchical designs.
equations_mixed <- function(result) {
  fit <- result$fit
  gaussian <- is_gaussian_fit(fit)
  coef_names <- names(lme4::fixef(fit))
  slopes <- setdiff(coef_names, "(Intercept)")
  grp <- equation_groups(fit)
  cnms <- lme4::getME(fit, "cnms")
  unit_letters <- c("j", "k", "l", "m", "n", "o")
  random_letters <- c("u", "v", "w", "z", "a", "d")
  variable_tex <- function(name) {
    components <- strsplit(name, ":", fixed = TRUE)[[1]]
    paste(sprintf("%s_{i}", latex_var(components)), collapse = "\\,")
  }
  outcome_tex <- sprintf("%s_{i}", latex_var(result$spec$outcome))
  lhs <- if (gaussian) outcome_tex else "\\eta_{i}"
  fixed_terms <- c(
    if ("(Intercept)" %in% coef_names) "\\beta_{0}",
    sprintf("\\beta_{%s}%s", seq_along(slopes), vapply(slopes, variable_tex, character(1)))
  )
  blocks <- lapply(seq_along(grp$groups), function(g) {
    group <- grp$groups[[g]]
    unit <- unit_letters[[(g - 1) %% length(unit_letters) + 1]]
    letter <- random_letters[[(g - 1) %% length(random_letters) + 1]]
    components <- cnms[names(cnms) == group]
    labels <- unlist(components, use.names = FALSE)
    component <- rep(seq_along(components), lengths(components))
    label_index <- match(labels, c("(Intercept)", slopes)) - 1L
    if (anyNA(label_index)) label_index <- seq_along(labels) - 1L
    symbols <- sprintf("%s_{%s%s}", letter, label_index, unit)
    n <- length(labels)
    cells <- matrix("0", nrow = n, ncol = n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (component[[i]] != component[[j]]) next
        pair <- c(label_index[[max(i, j)]], label_index[[min(i, j)]])
        cells[i, j] <- sprintf("\\tau_{%s %s}", letter, paste(pair, collapse = if (any(pair > 9)) "," else ""))
      }
    }
    terms <- paste0(
      sprintf("%s_{%s%s[i]}", letter, label_index, unit),
      vapply(labels, function(label) if (identical(label, "(Intercept)")) "" else variable_tex(label), character(1))
    )
    list(group = group, labels = labels, symbols = symbols, cells = cells, terms = terms)
  })
  equation <- paste(lhs, "=", paste(c(fixed_terms, unlist(lapply(blocks, `[[`, "terms")), if (gaussian) "e_{i}"), collapse = " + "))
  equations <- if (gaussian) equation else paste(equation_link_line(result, "\\eta_{i}", outcome_tex), equation, sep = "\\\\ ")
  key <- data.frame(
    Symbol = c(if ("(Intercept)" %in% coef_names) "beta_0", sprintf("beta_%s", seq_along(slopes))),
    Coefficient = c(if ("(Intercept)" %in% coef_names) "(Intercept)", slopes),
    stringsAsFactors = FALSE
  )
  key$Estimate <- unname(lme4::fixef(fit)[key$Coefficient])
  list(
    level = "Mixed-model form",
    equations = equations,
    combined = equation,
    blocks = lapply(blocks, function(block) block[c("group", "labels", "symbols", "cells")]),
    key = key,
    notation = "mixed-model"
  )
}

equation_structure <- function(result) {
  lev <- tryCatch(equation_levels(result$fit), error = function(e) NULL)
  if (is.null(lev)) equations_mixed(result) else equations_hierarchical(result, lev)
}

mlm_latex_equations <- function(result) {
  eq <- equation_structure(result)
  note <- switch(
    eq$notation,
    `mixed-model` = "The grouping factors are crossed or exceed three levels, so the model is written as a single mixed-model equation. Index i denotes observations and j[i], k[i] denote the units that observation i belongs to.",
    "Predictors are assigned to levels from the fitted data: a predictor that is constant within every unit of a grouping factor is treated as measured at that level. Cross-level interactions appear in the equation for the lower-level coefficient they moderate."
  )
  list(
    level = eq$level,
    equations = eq$equations,
    combined = eq$combined,
    tau = tau_latex(result, eq),
    key = eq$key,
    notation = eq$notation,
    note = note
  )
}

tau_blocks <- function(result, eq = equation_structure(result)) {
  eq$blocks
}

tau_latex <- function(result, eq = equation_structure(result)) {
  vapply(eq$blocks, function(block) {
    tau_block_latex(block$symbols, block$cells, block$group, block$labels)
  }, character(1))
}

apa_fixed_table <- function(fit, exponentiate = FALSE) {
  tab <- fixed_effects_table(fit)
  stat_col <- intersect(c("t value", "z value"), names(tab))
  stat <- if (length(stat_col)) tab[[stat_col[[1]]]] else tab$Estimate / tab$`Std. Error`
  out <- data.frame(
    Predictor = tab$term,
    b = sprintf("%.2f", tab$Estimate),
    SE = sprintf("%.2f", tab$`Std. Error`),
    Statistic = sprintf("%.2f", stat),
    p = ifelse(tab$p < .001, "< .001", sprintf("%.3f", tab$p)),
    `95% CI` = sprintf("[%.2f, %.2f]", tab$CI_low, tab$CI_high),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  label <- exponentiated_label(fit)
  if (isTRUE(exponentiate) && !is.null(label)) {
    out[[label]] <- sprintf("%.2f", exp(tab$Estimate))
    out[[sprintf("%s 95%% CI", label)]] <- sprintf("[%.2f, %.2f]", exp(tab$CI_low), exp(tab$CI_high))
  }
  out
}

latex_table_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\", "\001", x, fixed = TRUE)
  x <- gsub("([{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("\001", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("#", "\\#", x, fixed = TRUE)
  x <- gsub("$", "\\$", x, fixed = TRUE)
  x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
  x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("<", "$<$", x, fixed = TRUE)
  x <- gsub(">", "$>$", x, fixed = TRUE)
  x
}

apa_fixed_latex <- function(fit, caption = "Fixed effects from the multilevel model.", label = "tab:fixed-effects", exponentiate = FALSE) {
  tab <- apa_fixed_table(fit, exponentiate = exponentiate)
  colspec <- paste0("lrrrrp{1.45in}", if (ncol(tab) > 6) "rp{1.45in}" else "")
  apa_latex_table(tab, caption = caption, label = label, colspec = colspec)
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

render_latex_piece <- function(x) {
  x <- html_escape(x)
  x <- gsub("\\\\_", "_", x, fixed = FALSE)
  x <- gsub("\\\\left", "", x)
  x <- gsub("\\\\right", "", x)
  x <- gsub("\\\\quad", " ", x)
  x <- gsub("\\\\,", " ", x)
  x <- gsub("\\\\sim", "<span class='eq-op'>&sim;</span>", x)
  x <- gsub("\\\\cdot", "&middot;", x)
  x <- gsub("\\\\eta", "&eta;", x)
  x <- gsub("\\\\beta", "&beta;", x)
  x <- gsub("\\\\gamma", "&gamma;", x)
  x <- gsub("\\\\tau", "&tau;", x)
  x <- gsub("\\\\pi", "&pi;", x)
  x <- gsub("\\\\mathrm\\{([^{}]+)\\}", "<span class='eq-var'>\\1</span>", x, perl = TRUE)
  x <- gsub("\\\\text\\{([^{}]+)\\}", "<span class='eq-text'>\\1</span>", x, perl = TRUE)
  x <- gsub("_\\{([^{}]+)\\}", "<sub>\\1</sub>", x, perl = TRUE)
  x <- gsub("\\\\", "", x, fixed = TRUE)
  x
}

render_bmatrix_html <- function(content) {
  rows <- strsplit(content, "\\\\\\\\", perl = TRUE)[[1]]
  rows <- rows[nzchar(trimws(rows))]
  cells <- lapply(rows, function(row) {
    cols <- strsplit(row, "\\s*&\\s*", perl = TRUE)[[1]]
    paste(sprintf("<td>%s</td>", vapply(cols, render_latex_piece, character(1))), collapse = "")
  })
  paste0("<span class='eq-matrix-frame'><table class='eq-matrix' aria-label='Matrix'><tbody>",
         paste(sprintf("<tr>%s</tr>", unlist(cells, use.names = FALSE)), collapse = ""),
         "</tbody></table></span>")
}

replace_bmatrix_html <- function(x) {
  pattern <- "\\\\begin\\{bmatrix\\}(.+?)\\\\end\\{bmatrix\\}"
  repeat {
    match <- regexpr(pattern, x, perl = TRUE)
    if (match[[1]] < 0) break
    full <- regmatches(x, match)
    content <- sub(pattern, "\\1", full, perl = TRUE)
    replacement <- render_bmatrix_html(content)
    regmatches(x, match) <- replacement
  }
  x
}

render_latex_with_matrices <- function(x) {
  pattern <- "\\\\begin\\{bmatrix\\}(.+?)\\\\end\\{bmatrix\\}"
  out <- character()
  rest <- x
  repeat {
    match <- regexpr(pattern, rest, perl = TRUE)
    if (match[[1]] < 0) {
      out <- c(out, render_latex_piece(rest))
      break
    }
    start <- match[[1]]
    end <- start + attr(match, "match.length") - 1
    before <- substr(rest, 1, start - 1)
    full <- substr(rest, start, end)
    content <- sub(pattern, "\\1", full, perl = TRUE)
    out <- c(out, render_latex_piece(before), render_bmatrix_html(content))
    rest <- substr(rest, end + 1, nchar(rest))
  }
  paste(out, collapse = "")
}

equation_html <- function(latex) {
  parts <- unlist(lapply(latex, function(item) {
    if (grepl("\\\\begin\\{bmatrix\\}", item)) return(item)
    strsplit(item, "\\\\\\\\", perl = TRUE)[[1]]
  }), use.names = FALSE)
  parts <- parts[nzchar(trimws(parts))]
  rendered <- vapply(parts, function(part) {
    render_latex_with_matrices(part)
  }, character(1))
  paste0("<div class='equation-rendered'>", paste(sprintf("<div class='equation-line'>%s</div>", rendered), collapse = ""), "</div>")
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
    "% Table 5. Software and R packages",
    paste(apa_software_latex(), collapse = "\n"),
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

manuscript_report_markdown <- function(result, REML = NULL, optimizer = NULL, maxfun = NULL) {
  settings <- result_settings(result, REML, optimizer, maxfun)
  REML <- settings$REML
  optimizer <- settings$optimizer
  maxfun <- settings$maxfun
  summary <- model_summary_cards(result, optimizer)
  diagnostics <- mlm_diagnostics(result$fit)
  fixed <- apa_fixed_table(result$fit)
  variance <- variance_table(result$fit)
  icc <- icc_table(result$fit)
  software <- mlm_software_table()
  reproducibility <- mlm_reproducibility_check(result, REML = REML, optimizer = optimizer, maxfun = maxfun)
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
    "# Publication Readiness",
    "",
    "## Methods Text",
    "",
    mlm_methods_text(result, REML = REML, optimizer = optimizer, maxfun = maxfun),
    "",
    "## Reproducibility Checklist",
    "",
    table_md(reproducibility),
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
    "# Software and Packages",
    "",
    mlm_software_apa(),
    "",
    table_md(software),
    "",
    "## Optional papaja Citation Workflow",
    "",
    "If `papaja` is installed, the following code creates APA-style R/package citations and a BibTeX file for the packages used in this analysis.",
    "",
    "```r",
    mlm_papaja_code(),
    "```",
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
    "## Coefficient Key",
    "",
    eq$note,
    "",
    table_md(equation_key_table(result)),
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

apa_fixed_html <- function(fit, table_number = 1, title = "Fixed Effects From the Multilevel Model", exponentiate = FALSE) {
  tab <- apa_fixed_table(fit, exponentiate = exponentiate)
  apa_html_table(
    tab,
    table_number = table_number,
    title = title,
    note = html_escape(paste(
      "Confidence intervals are Wald 95% intervals.",
      fixed_effects_note(fit),
      if (ncol(tab) > 6) sprintf("%s = exponentiated coefficient, with its interval obtained by exponentiating the Wald limits.", names(tab)[[7]])
    )),
    italic_columns = c("b", "SE", "p")
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
    note = html_escape(icc_note(fit))
  )
}

apa_tables_html_document <- function(result, exponentiate = FALSE) {
  paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>mlmr APA Tables</title>",
    "<style>",
    "body{font-family:'Times New Roman',serif;max-width:980px;margin:32px auto;line-height:1.35;color:#111;}",
    ".apa-table-wrap{margin-bottom:28px;}.apa-table-number{font-weight:bold;margin-bottom:4px;}",
    ".apa-table-title{font-style:italic;margin-bottom:8px;}.apa-table{width:100%;border-collapse:collapse;border-top:1.5px solid currentColor;border-bottom:1.5px solid currentColor;}",
    ".apa-table thead{border-bottom:1px solid currentColor;}.apa-table th,.apa-table td{padding:6px 8px;border:none;vertical-align:top;text-align:right;}",
    ".apa-table th:first-child,.apa-table td:first-child{text-align:left;}.apa-table th{font-weight:normal;}.apa-table-note{margin-top:8px;}",
    "</style></head><body>",
    apa_fixed_html(result$fit, exponentiate = exponentiate),
    apa_dummy_html(result$data, names(result$spec$fixed)),
    apa_variance_html(result$fit),
    apa_icc_html(result$fit),
    apa_software_html(),
    "</body></html>"
  )
}

apa_tables_latex_document <- function(result, exponentiate = FALSE) {
  paste(
    "% mlmr APA manuscript tables",
    "% Recommended preamble: \\usepackage{graphicx}",
    paste(apa_fixed_latex(result$fit, exponentiate = exponentiate), collapse = "\n"),
    "",
    paste(apa_dummy_latex(result$data, names(result$spec$fixed)), collapse = "\n"),
    "",
    paste(apa_variance_latex(result$fit), collapse = "\n"),
    "",
    paste(apa_icc_latex(result$fit), collapse = "\n"),
    "",
    paste(apa_software_latex(), collapse = "\n"),
    sep = "\n"
  )
}

mlm_apa_tables <- function(result, format = c("list", "html", "latex"), exponentiate = FALSE) {
  format <- match.arg(format)
  if (identical(format, "html")) {
    return(apa_tables_html_document(result, exponentiate = exponentiate))
  }
  if (identical(format, "latex")) {
    return(apa_tables_latex_document(result, exponentiate = exponentiate))
  }
  list(
    fixed_effects = apa_fixed_table(result$fit, exponentiate = exponentiate),
    dummy_coding = dummy_coding_table(result$data, names(result$spec$fixed)),
    variance_components = variance_table(result$fit),
    icc = icc_table(result$fit),
    software = mlm_software_table()
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
    icc_note(fit)
  }
  model_text <- if (identical(spec$distribution, "gaussian")) {
    sprintf("A Gaussian multilevel linear model was estimated for %s using %s. The fitted model was specified as: %s.", outcome, if (isTRUE(lme4::getME(fit, "devcomp")$dims[["REML"]])) "restricted maximum likelihood" else "maximum likelihood", formula_text)
  } else {
    sprintf("A %s generalized mixed-effects model with a %s link was estimated for %s using maximum likelihood. The fitted model was specified as: %s.", spec$distribution, spec$link, outcome, formula_text)
  }
  paste(
    model_text,
    sprintf("The model included fixed effects for %s and random effects for %s. Random slope variation was estimated for %s, allowing the corresponding Level-1 associations to vary across higher-level units.", paste(names(spec$fixed), collapse = ", "), paste(groups, collapse = ", "), if (length(random_slopes)) paste(random_slopes, collapse = ", ") else "no predictors"),
    sprintf("The fixed-effect results indicated the following statistically detectable associations: %s. %s", sig_text, fixed_effects_note(fit)),
    sprintf("Variance partitioning suggested the following clustering structure: %s", icc_text),
    sprintf("Model diagnostics reported: %s. Singularity or convergence warnings should be interpreted as evidence that the specified random-effects structure may be too complex for the available data or may require alternative centering, simplification, or optimizer settings.", paste(sprintf("%s = %s", diag$check, diag$value), collapse = "; ")),
    "Substantively, the model separates within-unit predictors from higher-level contextual predictors while estimating heterogeneity in selected slopes. This is the central advantage of the multilevel specification over a single-level regression: it permits the average association, the between-group variance, and the group-to-group variation in key effects to be evaluated within one coherent model.",
    sep = "\n\n"
  )
}

mlm_reproducibility_check <- function(result, REML = NULL, optimizer = NULL, maxfun = NULL) {
  settings <- result_settings(result, REML, optimizer, maxfun)
  REML <- settings$REML
  optimizer <- settings$optimizer
  maxfun <- settings$maxfun
  spec <- result$spec
  diag <- mlm_diagnostics(result$fit)
  vars <- model_variables(spec)
  vars <- vars[vars %in% names(result$data)]
  complete_rows <- if (length(vars)) {
    sum(stats::complete.cases(result$data[, vars, drop = FALSE]))
  } else {
    nrow(result$data)
  }
  random_slopes <- unique(unlist(lapply(spec$random, function(x) x$slopes %||% character())))
  singular <- diag$value[diag$check == "Singular fit"]
  convergence <- diag$value[diag$check == "Convergence messages"]
  centering <- vapply(spec$fixed, function(x) x$center %||% "none", character(1))
  add <- function(status, item, evidence, next_step = "") {
    data.frame(
      Status = status,
      Item = item,
      Evidence = evidence,
      `Suggested Review` = next_step,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  rows <- list(
    add("OK", "Model formula captured", paste(deparse(result$formula, width.cutoff = 500), collapse = " "), "Confirm the formula matches the research question before publication."),
    add("OK", "Data footprint captured", sprintf("%s rows in fitted data; %s complete rows on selected model variables.", nrow(result$data), complete_rows), "Archive the raw data source and any cleaning script outside the app export."),
    add("OK", "Estimator settings captured", sprintf("Distribution: %s; link: %s; REML: %s; optimizer: %s; maxfun: %s.", spec$distribution, spec$link, REML, optimizer, maxfun), "Use ML rather than REML for nested fixed-effect comparisons."),
    add(if (length(centering) && any(centering != "none")) "OK" else "Review", "Centering decisions captured", if (length(centering)) paste(sprintf("%s = %s", names(centering), centering), collapse = "; ") else "No fixed predictors selected.", "Confirm centering choices match within-, between-, or contextual-effect claims."),
    add("OK", "Random-effects structure captured", sprintf("Groups: %s; random slopes: %s.", paste(names(spec$random), collapse = ", "), if (length(random_slopes)) paste(random_slopes, collapse = ", ") else "none"), "Report simplification decisions if convergence or singularity occurs."),
    add(if (identical(singular, "TRUE")) "Review" else "OK", "Singular-fit check", singular, "If TRUE, consider a simpler random-effects structure or explain why the boundary estimate is acceptable."),
    add(if (length(convergence) && !identical(convergence, "None reported")) "Review" else "OK", "Convergence messages", convergence, "Resolve convergence warnings before treating the model as final."),
    add("Review", "Inference labels reviewed", fixed_effects_note(result$fit), "Use denominator-df, bootstrap, profile-likelihood, or journal-preferred methods when confirmatory inference requires them."),
    add(if (is_gaussian_fit(result$fit)) "OK" else "Review", "ICC reporting reviewed", icc_note(result$fit), "For GLMMs, report a family-specific ICC only after selecting and documenting an appropriate scale."),
    add("OK", "Tables and equations generated", "APA tables, variance components, equations, LaTeX, and Quarto/R exports are available. ICCs are produced for Gaussian mixed models only.", "Check labels, reference categories, notation, and journal formatting manually."),
    add("OK", "Software versions captured", paste(sprintf("%s %s", mlm_software_table()$Software, mlm_software_table()$Version), collapse = "; "), "Include the software table or APA software statement in supplementary materials.")
  )
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

mlm_methods_text <- function(result, REML = NULL, optimizer = NULL, maxfun = NULL) {
  settings <- result_settings(result, REML, optimizer, maxfun)
  REML <- settings$REML
  optimizer <- settings$optimizer
  maxfun <- settings$maxfun
  spec <- result$spec
  formula_text <- paste(deparse(result$formula, width.cutoff = 500), collapse = " ")
  groups <- names(spec$random)
  random_slopes <- unique(unlist(lapply(spec$random, function(x) x$slopes %||% character())))
  fixed_terms <- names(spec$fixed)
  centering <- vapply(spec$fixed, function(x) x$center %||% "none", character(1))
  centered_terms <- centering[centering != "none"]
  distribution_text <- if (identical(spec$distribution, "gaussian")) {
    "Gaussian linear mixed-effects model"
  } else {
    sprintf("%s generalized mixed-effects model with a %s link", spec$distribution, spec$link)
  }
  estimator <- if (identical(spec$distribution, "gaussian")) {
    if (isTRUE(REML)) "restricted maximum likelihood" else "maximum likelihood"
  } else {
    "maximum likelihood"
  }
  paste(
    sprintf("We estimated a %s for %s using %s in lme4. The fitted formula was %s.", distribution_text, spec$outcome, estimator, formula_text),
    sprintf("The model included fixed effects for %s and random-effect blocks for %s. Random slopes were estimated for %s.", if (length(fixed_terms)) paste(fixed_terms, collapse = ", ") else "the intercept only", if (length(groups)) paste(groups, collapse = ", ") else "no grouping factors", if (length(random_slopes)) paste(random_slopes, collapse = ", ") else "no predictors"),
    sprintf("Centering choices were recorded as follows: %s.", if (length(centered_terms)) paste(sprintf("%s was %s centered", names(centered_terms), centered_terms), collapse = "; ") else "no selected predictors were centered"),
    fixed_effects_note(result$fit),
    icc_note(result$fit),
    sprintf("The analysis used optimizer %s with a maximum of %s function evaluations. Reproducible R code, model diagnostics, APA-style tables, equations, LaTeX output, and software version information were exported from mlmr.", optimizer, maxfun),
    mlm_software_apa(),
    sep = "\n\n"
  )
}

mlm_publication_bundle <- function(result, REML = NULL, optimizer = NULL, maxfun = NULL) {
  list(
    methods_text = mlm_methods_text(result, REML = REML, optimizer = optimizer, maxfun = maxfun),
    reproducibility_check = mlm_reproducibility_check(result, REML = REML, optimizer = optimizer, maxfun = maxfun),
    software_statement = mlm_software_apa(),
    formula = paste(deparse(result$formula, width.cutoff = 500), collapse = " "),
    apa_tables = mlm_apa_tables(result),
    equations = mlm_latex_equations(result),
    reproducible_code = generate_repro_code(result, REML = REML, optimizer = optimizer, maxfun = maxfun)
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

generate_repro_code <- function(result, REML = NULL, optimizer = NULL, maxfun = NULL) {
  settings <- result_settings(result, REML, optimizer, maxfun)
  REML <- settings$REML
  optimizer <- settings$optimizer
  maxfun <- settings$maxfun
  spec <- result$spec
  model_vars <- intersect(model_variables(spec), names(result$data))
  latex <- mlm_latex_equations(result)
  formula_text <- paste(deparse(result$formula, width.cutoff = 500), collapse = " ")
  factor_terms <- intersect(names(spec$fixed), factor_vars(result$data))
  section <- function(title) c(strrep("#", 72), paste("#", title), strrep("#", 72))
  c(
    section("Packages"),
    "library(lme4)",
    "",
    "# These packages are reported in the manuscript software section.",
    "reported_packages <- c(\"R\", \"mlmr\", \"lme4\", \"shiny\", \"bslib\", \"ggplot2\")",
    "",
    section("Model Structure"),
    sprintf("# Grouping structure selected in the app: %s", spec$structure %||% "nested"),
    sprintf("# Grouping factors: %s", paste(unlist(spec$grouping, use.names = FALSE), collapse = ", ")),
    sprintf("# Structure note: %s", spec$nesting %||% ""),
    "",
    section("Analysis Sample"),
    "# `dat` must hold the data used in mlmr. Rows with missing values on any",
    "# model variable are dropped first, so centering uses the same rows as the model.",
    sprintf("model_vars <- c(%s)", paste(sprintf("\"%s\"", model_vars), collapse = ", ")),
    "dat_full <- dat",
    "dat <- dat_full[complete.cases(dat_full[, model_vars, drop = FALSE]), , drop = FALSE]",
    "",
    section("Center Predictors"),
    if (length(result$centering_code)) "# Center predictors as requested in mlmr." else "# No predictors were centered.",
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
    section("Table 5: Software and R Packages"),
    "# Table 5. Software and package versions used for the analysis",
    software_repro_code(),
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
    imputation_repro_code(spec, result$formula, data_name = "dat_full")
  )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

first_or <- function(x, y = NULL) {
  if (is.null(x) || !length(x)) y else x[[1]]
}
