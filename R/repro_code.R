software_repro_code <- function(packages = mlm_default_packages()) {
  packages <- unique(packages[nzchar(packages)])
  c(
    r_char_vector_lines("reported_packages", packages),
    "package_versions <- data.frame(",
    "  Software = reported_packages,",
    "  Version = vapply(reported_packages, function(pkg) {",
    "    if (identical(pkg, \"R\")) {",
    "      paste(R.version$major, R.version$minor, sep = \".\")",
    "    } else if (requireNamespace(pkg, quietly = TRUE)) {",
    "      as.character(utils::packageVersion(pkg))",
    "    } else {",
    "      NA_character_",
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

fit_code_lines <- function(spec, formula_text, REML, optimizer, maxfun, df_method = "none") {
  if (identical(spec$distribution, "gaussian")) {
    if (df_method %in% c("satterthwaite", "kenward-roger")) {
      return(c(
        sprintf("lmer_fn <- if (requireNamespace(\"lmerTest\", quietly = TRUE)) lmerTest::lmer else lme4::lmer"),
        sprintf("fit <- lmer_fn(%s, data = dat, REML = %s,", formula_text, if (isTRUE(REML)) "TRUE" else "FALSE"),
        sprintf("               control = lmerControl(optimizer = %s, optCtrl = list(maxfun = %s)))", r_string_literal(optimizer), maxfun)
      ))
    }
    return(c(
      sprintf("fit <- lmer(%s, data = dat, REML = %s,", formula_text, if (isTRUE(REML)) "TRUE" else "FALSE"),
      sprintf("            control = lmerControl(optimizer = %s, optCtrl = list(maxfun = %s)))", r_string_literal(optimizer), maxfun)
    ))
  }
  control_line <- sprintf("glmerControl(optimizer = %s, optCtrl = list(maxfun = %s))", r_string_literal(optimizer), maxfun)
  if (identical(spec$distribution, "negative binomial")) {
    return(c(
      sprintf("fit <- glmer.nb(%s, data = dat,", formula_text),
      sprintf("                control = %s)", control_line)
    ))
  }
  family_call <- switch(
    spec$distribution,
    binomial = sprintf("binomial(link = %s)", r_string_literal(spec$link)),
    poisson = sprintf("poisson(link = %s)", r_string_literal(spec$link)),
    Gamma = sprintf("Gamma(link = %s)", r_string_literal(spec$link)),
    sprintf("%s(link = %s)", spec$distribution, r_string_literal(spec$link))
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
  df_method <- settings$df_method
  spec <- result$spec
  pkgs <- active_packages_for_result(result)
  model_vars <- intersect(model_variables(spec), names(result$data))
  latex <- mlm_latex_equations(result)
  formula_text <- paste(deparse(result$formula, width.cutoff = 500), collapse = " ")
  factor_terms <- intersect(names(spec$fixed), factor_vars(result$data))
  section <- function(title) c(strrep("#", 72), paste("#", title), strrep("#", 72))

  table1_code <- if (is_gaussian_fit(result$fit) && df_method %in% c("satterthwaite", "kenward-roger")) {
    ddf_arg <- if (identical(df_method, "kenward-roger")) "Kenward-Roger" else "Satterthwaite"
    c(
      "if (inherits(fit, \"lmerModLmerTest\")) {",
      sprintf("  coefs <- coef(summary(fit, ddf = %s))", r_string_literal(ddf_arg)),
      "  crit <- qt(.975, df = pmax(coefs[, \"df\"], 1))",
      "  apa_table <- data.frame(",
      "    Predictor = rownames(coefs),",
      "    b = sprintf(\"%.2f\", coefs[, \"Estimate\"]),",
      "    SE = sprintf(\"%.2f\", coefs[, \"Std. Error\"]),",
      "    df = sprintf(\"%.1f\", coefs[, \"df\"]),",
      "    t = sprintf(\"%.2f\", coefs[, \"t value\"]),",
      "    p = fmt_p(coefs[, \"Pr(>|t|)\"]),",
      "    `95% CI` = sprintf(\"[%.2f, %.2f]\", coefs[, \"Estimate\"] - crit * coefs[, \"Std. Error\"], coefs[, \"Estimate\"] + crit * coefs[, \"Std. Error\"]),",
      "    check.names = FALSE",
      "  )",
      "} else {",
      "  coefs <- coef(summary(fit))",
      "  stat <- coefs[, \"t value\"]",
      "  p <- 2 * pnorm(abs(stat), lower.tail = FALSE)",
      "  apa_table <- data.frame(",
      "    Predictor = rownames(coefs),",
      "    b = sprintf(\"%.2f\", coefs[, \"Estimate\"]),",
      "    SE = sprintf(\"%.2f\", coefs[, \"Std. Error\"]),",
      "    Statistic = sprintf(\"%.2f\", stat),",
      "    p = fmt_p(p),",
      "    `95% CI` = sprintf(\"[%.2f, %.2f]\", coefs[, \"Estimate\"] - qnorm(.975) * coefs[, \"Std. Error\"], coefs[, \"Estimate\"] + qnorm(.975) * coefs[, \"Std. Error\"]),",
      "    check.names = FALSE",
      "  )",
      "}",
      "apa_table"
    )
  } else {
    c(
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
      "apa_table"
    )
  }

  c(
    section("Packages"),
    "library(lme4)",
    "",
    "# These packages are reported in the manuscript software section.",
    r_char_vector_lines("reported_packages", pkgs),
    "",
    section("Model Structure"),
    sprintf("# Grouping structure selected in the app: %s", spec$structure %||% "nested"),
    sprintf("# Grouping factors: %s", paste(unlist(spec$grouping, use.names = FALSE), collapse = ", ")),
    sprintf("# Structure note: %s", spec$nesting %||% ""),
    "",
    section("Analysis Sample"),
    "# `dat` must hold the data used in mlmr. Rows with missing values on any",
    "# model variable are dropped first, so centering uses the same rows as the model.",
    r_char_vector_lines("model_vars", model_vars),
    "dat_full <- dat",
    "dat <- dat_full[stats::complete.cases(dat_full[, model_vars, drop = FALSE]), , drop = FALSE]",
    "",
    section("Center Predictors"),
    if (length(result$centering_code)) "# Center predictors as requested in mlmr." else "# No predictors were centered.",
    result$centering_code,
    "",
    section("Fit Multilevel Model"),
    fit_code_lines(spec, formula_text, REML, optimizer, maxfun, df_method = df_method),
    "",
    "summary(fit)",
    "",
    section("Table Helpers"),
    "# Helper functions for manuscript-style tables (APA 7th p-value formatting)",
    "fmt_p <- function(p) {",
    "  ifelse(p < .001, \"< .001\", ifelse(p >= .9995, \"> .999\", sub(\"^0\\\\.\", \".\", sprintf(\"%.3f\", p))))",
    "}",
    "",
    section("Table 1: APA Fixed Effects"),
    "# Table 1. APA-style fixed effects",
    table1_code,
    "",
    section("Table 2: Dummy Coding"),
    "# Table 2. Dummy coding for categorical predictors",
    r_char_vector_lines("factor_terms", factor_terms),
    "dummy_table <- if (length(factor_terms)) {",
    "  do.call(rbind, lapply(factor_terms, function(term) {",
    "    x <- factor(dat[[term]])",
    "    data.frame(",
    "      Variable = term,",
    "      `Reference Category` = levels(x)[1],",
    "      `Estimated Contrasts` = paste(levels(x)[-1], collapse = \", \"),",
    "      check.names = FALSE",
    "    )",
    "  }))",
    "} else {",
    "  data.frame(Variable = character(), `Reference Category` = character(), `Estimated Contrasts` = character(), check.names = FALSE)",
    "}",
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
    "# Table 4. Intraclass correlations (Gaussian mixed models only)",
    "if (inherits(fit, \"lmerMod\") && !inherits(fit, \"glmerMod\")) {",
    "  vc <- as.data.frame(VarCorr(fit))",
    "  intercepts <- vc[vc$grp != \"Residual\" & is.na(vc$var2) & !is.na(vc$var1) & vc$var1 == \"(Intercept)\", c(\"grp\", \"vcov\"), drop = FALSE]",
    "  residual_var <- attr(VarCorr(fit), \"sc\")^2",
    "  total_var <- sum(intercepts$vcov, na.rm = TRUE) + residual_var",
    "  icc_table <- data.frame(",
    "    `Grouping Level` = intercepts$grp,",
    "    Variance = round(intercepts$vcov, 4),",
    "    ICC = round(intercepts$vcov / total_var, 4),",
    "    check.names = FALSE",
    "  )",
    "} else {",
    "  icc_table <- data.frame(`Grouping Level` = character(), Variance = numeric(), ICC = numeric(), check.names = FALSE)",
    "}",
    "icc_table",
    "",
    section("Table 5: Software and R Packages"),
    "# Table 5. Software and package versions used for the analysis",
    software_repro_code(pkgs),
    "",
    section("APA LaTeX Table"),
    "# Manuscript-ready LaTeX table",
    r_char_vector_lines("fixed_effects_latex_lines", apa_fixed_latex(result, exponentiate = TRUE)),
    "fixed_effects_latex <- paste(fixed_effects_latex_lines, collapse = \"\\n\")",
    "cat(fixed_effects_latex)",
    "",
    section("Empirical Bayes Estimates"),
    "# Empirical Bayes estimates",
    "ranef(fit)",
    "",
    section("Raw LaTeX Equations"),
    "# Raw LaTeX equations generated by mlmr",
    r_char_vector_lines("level_equations", latex$equations),
    sprintf("combined_equation <- %s", r_string_literal(latex$combined)),
    r_char_vector_lines("tau_matrices", latex$tau),
    "",
    imputation_repro_code(spec, result$formula, data_name = "dat_full")
  )
}
