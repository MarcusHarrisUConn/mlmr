group_structure_table <- function(data, groups, structure = "nested") {
  groups <- groups[groups %in% names(data)]
  if (!length(groups)) {
    return(data.frame(
      Grouping = character(),
      `Number of Units` = integer(),
      `Rows per Unit` = character(),
      Structure = character(),
      check.names = FALSE
    ))
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
    return(sprintf(
      "Crossed/non-nested structure: lme4 estimates separate random-effect blocks such as %s. This is the pattern for students taught by multiple teachers or patients connected to hospitals and doctors that do not form one strict hierarchy.",
      blocks
    ))
  }
  if (identical(structure, "multiple membership")) {
    return(sprintf(
      "Multiple-membership structure: this prototype records the design intent and generates code scaffolds, but weighted membership terms require later support beyond basic lme4 syntax. Current random-effect blocks are shown as %s.",
      blocks
    ))
  }
  sprintf(
    "Nested structure: the selected grouping factors are treated as a hierarchy or nested design note. lme4 will estimate the random-effect blocks %s.",
    blocks
  )
}

variable_role_table <- function(data, spec) {
  labels <- variable_labels_map(data)
  has_labels <- any(nzchar(labels))
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
    row <- data.frame(
      Variable = var,
      Type = if (is.numeric(data[[var]])) "numeric" else if (is.factor(data[[var]]) || is.character(data[[var]])) "categorical" else class(data[[var]])[[1]],
      Role = paste(unique(out), collapse = ", "),
      `Unique Values` = length(unique(data[[var]][!is.na(data[[var]])])),
      Missing = sum(is.na(data[[var]])),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    if (has_labels) {
      row$Label <- labels[[var]] %||% ""
      row <- row[, c("Variable", "Label", "Type", "Role", "Unique Values", "Missing"), drop = FALSE]
    }
    row
  })
  do.call(rbind, roles)
}

model_builder_summary <- function(spec) {
  fixed <- names(spec$fixed)
  random <- names(spec$random)
  slopes <- unique(unlist(lapply(spec$random, function(x) {
    s <- x$slopes %||% character()
    s
  })))
  slope_summary <- unlist(lapply(names(spec$random), function(g) {
    s <- spec$random[[g]]$slopes %||% character()
    if (length(s)) sprintf("%s (%s)", paste(s, collapse = ", "), g) else character()
  }))
  data.frame(
    Component = c("Outcome", "Distribution", "Fixed predictors", "Interactions", "Grouping factors", "Random slopes", "Structure"),
    Selection = c(
      spec$outcome,
      if (identical(spec$distribution, "gaussian")) "Gaussian" else sprintf("%s (%s)", spec$distribution, spec$link),
      if (length(fixed)) paste(fixed, collapse = ", ") else "Intercept-only",
      if (length(spec$interactions)) paste(vapply(spec$interactions, paste, character(1), collapse = ":"), collapse = ", ") else "None",
      if (length(random)) paste(random, collapse = ", ") else "None",
      if (length(slope_summary)) paste(slope_summary, collapse = "; ") else if (length(slopes)) paste(slopes, collapse = ", ") else "None",
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

  groups <- intersect(unlist(spec$grouping, use.names = FALSE), names(data))
  declared_levels <- spec$predictor_levels %||% list()
  declared_level <- function(var) {
    hit <- names(Filter(function(x) var %in% x, declared_levels))
    if (length(hit)) hit[[1]] else "Not assigned"
  }
  within_text <- function(var) {
    if (!var %in% names(data) || !length(groups)) return("Not checked")
    pieces <- vapply(groups, function(grp) {
      keep <- !is.na(data[[grp]])
      if (!any(keep)) return(sprintf("No valid %s IDs", grp))
      within_counts <- tapply(
        data[[var]][keep],
        as.character(data[[grp]][keep]),
        function(x) length(unique(x[!is.na(x)]))
      )
      if (all(within_counts <= 1, na.rm = TRUE)) {
        sprintf("Constant within %s", grp)
      } else {
        sprintf("Varies within %s", grp)
      }
    }, character(1))
    paste(pieces, collapse = "; ")
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

missingness_table <- function(data) {
  if (!ncol(data)) {
    return(data.frame(Variable = character(), Missing = integer(), `Percent Missing` = character(), check.names = FALSE))
  }
  miss_counts <- vapply(data, function(x) sum(is.na(x)), integer(1))
  pct <- if (nrow(data) > 0) 100 * miss_counts / nrow(data) else rep(0, length(miss_counts))
  out <- data.frame(
    Variable = names(data),
    Missing = miss_counts,
    `Percent Missing` = sprintf("%.1f%%", pct),
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  out[order(out$Missing, decreasing = TRUE), , drop = FALSE]
}

model_missingness <- function(data, spec) {
  vars <- model_variables(spec)
  vars <- vars[vars %in% names(data)]
  if (!length(vars)) {
    return(data.frame(
      `Rows in data` = nrow(data),
      `Rows complete for model` = nrow(data),
      `Rows with missing model data` = 0L,
      check.names = FALSE
    ))
  }
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
  pct <- vapply(data[vars], function(x) if (length(x)) mean(is.na(x)) else 0, numeric(1))
  data.frame(
    Variable = vars,
    Role = vapply(vars, role_for, character(1)),
    Missing = miss,
    `Percent Missing` = sprintf("%.1f%%", 100 * pct),
    check.names = FALSE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
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
    add("Stop", "Outcome type", "Binomial models require a two-category outcome (binary 0/1 or two-level factor).")
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

  # Predictor variation and high-cardinality checks (Issue #1).
  for (pred in intersect(names(spec$fixed), names(data))) {
    vals <- stats::na.omit(data[[pred]])
    n_unique <- length(unique(vals))
    if (n_unique < 2) {
      add("Stop", paste("Predictor variation:", pred), sprintf("%s has fewer than two distinct non-missing values and cannot be estimated.", pred))
    } else if (is.numeric(vals) && stats::sd(vals) < 1e-6) {
      add("Warn", paste("Predictor variation:", pred), sprintf("%s has near-zero variance (SD < 1e-6); check scaling or collinearity before fitting.", pred))
    } else if ((is.factor(vals) || is.character(vals)) && (n_unique > 15 && n_unique >= floor(nrow(data) / 4))) {
      add(
        "Warn",
        paste("High-cardinality predictor:", pred),
        sprintf("%s has %s categories; confirm this is a substantive categorical predictor rather than a unit ID.", pred, n_unique)
      )
    }
  }

  # Interaction main-effects check (Issue #1).
  if (length(spec$interactions)) {
    inter_vars <- unique(unlist(spec$interactions, use.names = FALSE))
    omitted_main <- setdiff(inter_vars, names(spec$fixed))
    if (length(omitted_main)) {
      add(
        "Warn",
        "Interaction main effects",
        sprintf(
          "Interaction component(s) %s are not included as main effects in `fixed`. Omitting lower-order main effects constrains them to zero and is rarely intended.",
          paste(omitted_main, collapse = ", ")
        )
      )
    }
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
      raw_g <- stats::na.omit(data[[group]])
      n_group <- length(unique(raw_g))
      if (is.numeric(raw_g) && length(raw_g) > 0 &&
          (any(abs(raw_g - round(raw_g)) > 1e-8) || (n_group > 30 && n_group > 0.6 * length(raw_g)))) {
        add(
          "Warn",
          paste("Grouping coding:", group),
          sprintf("%s is numeric with %s unique values (including non-integers or near-unique values); confirm it represents discrete cluster IDs rather than a continuous predictor.", group, n_group)
        )
      }
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
      target_blocks <- names(Filter(function(r) slope %in% (r$slopes %||% character()), spec$random))
      slope_groups <- unique(vapply(target_blocks, function(b) spec$grouping[[b]] %||% b, character(1)))
      slope_groups <- intersect(if (length(slope_groups)) slope_groups else groups, names(data))
      for (group in slope_groups) {
        keep <- !is.na(data[[group]])
        if (!any(keep)) next
        counts <- tapply(data[[slope]][keep], as.character(data[[group]][keep]), function(x) length(unique(x[!is.na(x)])))
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
    keep_out <- !is.na(data[[spec$outcome]])
    outcome_values <- data[[spec$outcome]][keep_out]
    for (group in groups[groups %in% names(data)]) {
      keep <- keep_out & !is.na(data[[group]])
      if (!any(keep)) next
      group_outcomes <- tapply(data[[spec$outcome]][keep], as.character(data[[group]][keep]), function(x) length(unique(x)))
      one_class <- sum(group_outcomes <= 1, na.rm = TRUE)
      if (one_class > 0) {
        add("Warn", paste("Binomial separation:", group), sprintf("%s of %s %s units contain only one outcome category; check separation and sparse cells before reporting.", one_class, length(group_outcomes), group))
      } else {
        add("OK", paste("Binomial separation:", group), sprintf("Both outcome categories appear within all observed %s units.", group))
      }
    }
  }

  model_vars <- vars[vars %in% names(data)]
  if (length(model_vars)) {
    complete <- stats::complete.cases(data[, model_vars, drop = FALSE])
    pct_missing <- if (length(complete)) 100 * mean(!complete) else 0
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
      check.names = FALSE,
      stringsAsFactors = FALSE
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
    r_char_vector_lines("model_vars", vars),
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


# Auto-classifies every variable in `data` relative to the active clustering
# hierarchy so users can immediately see Dependent Variable, Cluster IDs,
# Level-1 IVs (vary within clusters), and Level-2/3 IVs (constant within clusters).
auto_classify_variables_table <- function(data, spec) {
  groups <- intersect(unlist(spec$grouping, use.names = FALSE), names(data))
  # Order groups from most clusters (Level 2) to fewest clusters (Level 3)
  if (length(groups) > 1L) {
    n_u <- vapply(groups, function(g) length(unique(stats::na.omit(data[[g]]))), integer(1))
    groups <- groups[order(-n_u)]
  }
  infer_empirical_level <- function(var) {
    if (identical(var, spec$outcome)) return("Dependent Variable (Y)")
    if (var %in% groups) {
      pos <- match(var, groups)
      if (identical(spec$structure, "crossed")) return(sprintf("Crossed Cluster ID (%s)", pos))
      return(sprintf("Level-%s Cluster ID", pos + 1L))
    }
    col <- data[[var]]
    n_uniq <- length(unique(col[!is.na(col)]))
    if (n_uniq == nrow(data) && (is.character(col) || is.factor(col))) {
      return("Observation / Row ID")
    }
    if (!length(groups)) return("Predictor candidate")
    if (!constant_within(col, data[[groups[[1]]]])) {
      return(sprintf("Level-1 candidate (varies within %s)", groups[[1]]))
    }
    if (length(groups) >= 2L && !constant_within(col, data[[groups[[2]]]])) {
      return(sprintf("Level-2 candidate (constant in %s; varies in %s)", groups[[1]], groups[[2]]))
    }
    if (length(groups) >= 2L) {
      return(sprintf("Level-3 candidate (constant in %s & %s)", groups[[1]], groups[[2]]))
    }
    sprintf("Level-2 candidate (constant within %s)", groups[[1]])
  }

  active_role <- function(var) {
    if (identical(var, spec$outcome)) return(sprintf("Outcome (%s)", spec$distribution))
    if (var %in% groups) return("Grouping / Random-effect ID")
    if (var %in% names(spec$fixed)) {
      ctr <- spec$fixed[[var]]$center %||% "none"
      ctr_lbl <- switch(ctr, CWC = "Group-mean centered (CWC)", GMC = "Grand-mean centered (GMC)", "Uncentered")
      sl_grps <- names(Filter(function(r) var %in% (r$slopes %||% character()), spec$random))
      sl_lbl <- if (length(sl_grps)) sprintf("; random slope across %s", paste(sl_grps, collapse = ", ")) else ""
      mods <- unlist(lapply(spec$interactions, function(p) if (var %in% p) setdiff(p, var) else character()))
      mod_lbl <- if (length(mods)) sprintf("; interacts with %s", paste(unique(mods), collapse = ", ")) else ""
      return(paste0("Independent Variable (", ctr_lbl, sl_lbl, mod_lbl, ")"))
    }
    "Not in current model"
  }

  do.call(rbind, lapply(names(data), function(var) {
    col <- data[[var]]
    type_str <- if (is.numeric(col)) {
      sprintf("Numeric (M = %.2f, SD = %.2f)", mean(col, na.rm = TRUE), stats::sd(col, na.rm = TRUE))
    } else {
      sprintf("Categorical (%s levels)", length(unique(col[!is.na(col)])))
    }
    data.frame(
      Variable = var,
      Type = type_str,
      `Empirical Level in Data` = infer_empirical_level(var),
      `Current Model Role` = active_role(var),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }))
}

# Concise table of ONLY the active Independent Variables (Predictors) in `spec`,
# displayed prominently on Data > Overview so DV and IVs are obvious at launch.
active_predictors_blueprint_table <- function(data, spec) {
  fixed <- names(spec$fixed)
  if (!length(fixed)) {
    return(data.frame(
      `Independent Variable (IV)` = "(Intercept-only / unconditional model)",
      `Declared Level` = "-",
      Centering = "-",
      `Slope / Moderation` = "Random intercept only; add predictors on the Model tab",
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  declared <- spec$predictor_levels %||% list()
  level_of <- function(v) {
    hit <- names(Filter(function(xs) v %in% xs, declared))
    if (length(hit)) hit[[1]] else "Assigned from data"
  }
  do.call(rbind, lapply(fixed, function(v) {
    ctr <- spec$fixed[[v]]$center %||% "none"
    ctr_text <- if (v %in% names(data) && !is.numeric(data[[v]])) {
      sprintf("Factor (ref: %s)", first_or(levels(factor(data[[v]])), ""))
    } else {
      switch(ctr, CWC = "Group-mean (CWC)", GMC = "Grand-mean (GMC)", "Uncentered")
    }
    sl_grps <- names(Filter(function(r) v %in% (r$slopes %||% character()), spec$random))
    partners <- unique(unlist(lapply(spec$interactions, function(p) if (v %in% p) setdiff(p, v) else character())))
    role_parts <- c(
      if (length(sl_grps)) sprintf("Random slope across %s", paste(sl_grps, collapse = ", ")) else "Fixed effect",
      if (length(partners)) sprintf("interacts with %s", paste(partners, collapse = ", "))
    )
    data.frame(
      `Independent Variable (IV)` = v,
      `Declared Level` = level_of(v),
      Centering = ctr_text,
      `Slope / Moderation` = paste(role_parts, collapse = "; "),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }))
}
