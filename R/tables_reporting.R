mlm_default_packages <- function() {
  c("R", "mlmr", "lme4", "shiny", "bslib", "ggplot2")
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
    lmerTest = "Denominator degrees of freedom and mixed-model tests",
    pbkrtest = "Kenward-Roger degrees-of-freedom approximation",
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
    lmerTest = "Kuznetsova et al.",
    pbkrtest = "Halekoh and Hojsgaard",
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

active_packages_for_result <- function(result = NULL) {
  pkgs <- mlm_default_packages()
  if (inherits(result, "mlm_result")) {
    df_m <- result$settings$df_method %||% "none"
    if (identical(df_m, "satterthwaite")) pkgs <- unique(c(pkgs, "lmerTest"))
    if (identical(df_m, "kenward-roger")) pkgs <- unique(c(pkgs, "lmerTest", "pbkrtest"))
  }
  pkgs
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
    sprintf("  papaja::r_refs(file = %s)", r_string_literal(bib_file)),
    "  software_citation <- papaja::cite_r(",
    sprintf("    file = %s,", r_string_literal(bib_file)),
    "    pkgs = reported_packages,",
    "    omit = FALSE",
    "  )",
    "  software_citation",
    "} else {",
    "  message(\"Install papaja to generate APA-style R/package citations and BibTeX references.\")",
    "}"
  )
}

exponentiated_label <- function(fit) {
  fit <- unwrap_fit(fit)
  if (is_gaussian_fit(fit)) return(NULL)
  fam <- stats::family(fit)
  if (identical(fam$link, "logit")) return("OR")
  if (identical(fam$link, "log")) {
    return(if (grepl("^(poisson|Negative Binomial)", fam$family)) "IRR" else "exp(b)")
  }
  NULL
}

apa_fixed_table <- function(x, exponentiate = FALSE, df_method = NULL) {
  fit <- unwrap_fit(x)
  tab <- fixed_effects_table(x, df_method = df_method)
  stat_col <- intersect(c("t value", "z value"), names(tab))
  stat <- if (length(stat_col)) tab[[stat_col[[1]]]] else tab$Estimate / tab$`Std. Error`
  has_df <- "df" %in% names(tab)
  out <- if (has_df) {
    data.frame(
      Predictor = tab$term,
      b = sprintf("%.2f", tab$Estimate),
      SE = sprintf("%.2f", tab$`Std. Error`),
      df = sprintf("%.1f", tab$df),
      t = sprintf("%.2f", stat),
      p = format_apa_p(tab$p),
      `95% CI` = sprintf("[%.2f, %.2f]", tab$CI_low, tab$CI_high),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      Predictor = tab$term,
      b = sprintf("%.2f", tab$Estimate),
      SE = sprintf("%.2f", tab$`Std. Error`),
      Statistic = sprintf("%.2f", stat),
      p = format_apa_p(tab$p),
      `95% CI` = sprintf("[%.2f, %.2f]", tab$CI_low, tab$CI_high),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  label <- exponentiated_label(fit)
  if (isTRUE(exponentiate) && !is.null(label)) {
    out[[label]] <- sprintf("%.2f", exp(tab$Estimate))
    out[[sprintf("%s 95%% CI", label)]] <- sprintf("[%.2f, %.2f]", exp(tab$CI_low), exp(tab$CI_high))
  }
  out
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

apa_variance_formatted_table <- function(fit) {
  tab <- variance_table(fit)
  names(tab) <- c("Group", "Effect", "Covarying Effect", "Variance/Covariance", "SD/Correlation")[seq_along(names(tab))]
  numeric_cols <- vapply(tab, is.numeric, logical(1))
  tab[numeric_cols] <- lapply(tab[numeric_cols], function(x) sprintf("%.3f", x))
  tab[is.na(tab)] <- ""
  tab
}

apa_latex_table <- function(tab, caption, label, colspec = NULL) {
  if (!nrow(tab)) {
    colspec <- colspec %||% paste0("l", paste(rep("r", max(0, ncol(tab) - 1)), collapse = ""))
    return(c(
      "\\begin{table}[ht]",
      "\\centering",
      "\\small",
      sprintf("\\caption{%s}", latex_table_escape(caption)),
      sprintf("\\label{%s}", label),
      sprintf("\\begin{tabular}{%s}", colspec),
      "\\hline",
      paste0(paste(latex_table_escape(names(tab)), collapse = " & "), " \\\\"),
      "\\hline",
      sprintf("\\multicolumn{%s}{l}{\\emph{No rows applicable for this model.}} \\\\", ncol(tab)),
      "\\hline",
      "\\end{tabular}",
      "\\end{table}"
    ))
  }
  tab[] <- lapply(tab, latex_table_escape)
  rows <- apply(tab, 1, function(x) paste(x, collapse = " & "))
  if (is.null(colspec)) {
    colspec <- paste0("l", paste(rep("r", max(0, ncol(tab) - 1)), collapse = ""))
  }
  c(
    "\\begin{table}[ht]",
    "\\centering",
    "\\small",
    sprintf("\\caption{%s}", latex_table_escape(caption)),
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

apa_fixed_latex <- function(x, caption = "Fixed effects from the multilevel model.", label = "tab:fixed-effects", exponentiate = FALSE) {
  tab <- apa_fixed_table(x, exponentiate = exponentiate)
  has_df <- "df" %in% names(tab)
  base_cols <- if (has_df) 7L else 6L
  base_spec <- if (has_df) "lrrrrrp{1.45in}" else "lrrrrp{1.45in}"
  colspec <- paste0(base_spec, if (ncol(tab) > base_cols) "rp{1.45in}" else "")
  apa_latex_table(tab, caption = caption, label = label, colspec = colspec)
}

apa_dummy_latex <- function(data, terms, caption = "Dummy coding for categorical predictors.", label = "tab:dummy-coding") {
  tab <- dummy_coding_table(data, terms)
  names(tab) <- c("Variable", "Reference Category", "Estimated Contrasts")
  apa_latex_table(tab, caption = caption, label = label, colspec = "lll")
}

apa_variance_latex <- function(fit, caption = "Random effects and variance components.", label = "tab:variance-components") {
  tab <- apa_variance_formatted_table(fit)
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

apa_software_latex <- function(packages = mlm_default_packages(), caption = "Software and R packages used in the analysis.", label = "tab:software") {
  tab <- mlm_software_table(packages)
  apa_latex_table(tab, caption = caption, label = label)
}

apa_html_table <- function(tab, table_number, title, note = NULL, italic_columns = character()) {
  if (!nrow(tab)) {
    return(paste0(
      "<div class='apa-table-wrap'>",
      sprintf("<div class='apa-table-number'>Table %s</div>", table_number),
      sprintf("<div class='apa-table-title'>%s</div>", html_escape(title)),
      "<div class='apa-empty'>No rows are available for this table.</div>",
      if (!is.null(note) && nzchar(note)) sprintf("<div class='apa-table-note'><em>Note.</em> %s</div>", note) else "",
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

apa_fixed_html <- function(x, table_number = 1, title = "Fixed Effects From the Multilevel Model", exponentiate = FALSE) {
  tab <- apa_fixed_table(x, exponentiate = exponentiate)
  has_df <- "df" %in% names(tab)
  base_cols <- if (has_df) 7L else 6L
  ci_label <- if (has_df) "Confidence intervals use t critical values with the reported degrees of freedom." else "Confidence intervals are Wald 95% intervals."
  apa_html_table(
    tab,
    table_number = table_number,
    title = title,
    note = html_escape(paste(
      ci_label,
      fixed_effects_note(x),
      if (ncol(tab) > base_cols) sprintf("%s = exponentiated coefficient, with its interval obtained by exponentiating the confidence limits.", names(tab)[[base_cols + 1L]])
    )),
    italic_columns = c("b", "SE", "df", "t", "p")
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
  tab <- apa_variance_formatted_table(fit)
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

apa_r2_html <- function(result, table_number = 5, title = "Model Variance Explained and Reliability") {
  r2 <- mlm_r2(result)
  rel <- mlm_reliability(result)
  r2_disp <- data.frame(
    `Marginal R2` = if (is.finite(r2$R2_marginal)) sprintf("%.3f", r2$R2_marginal) else "NA",
    `Conditional R2` = if (is.finite(r2$R2_conditional)) sprintf("%.3f", r2$R2_conditional) else "NA",
    `Fixed Var.` = sprintf("%.3f", r2$Fixed_Variance),
    `Random Var.` = sprintf("%.3f", r2$Random_Variance),
    Scale = r2$Scale,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rel_note <- if (nrow(rel)) {
    paste(
      "Empirical Bayes random-coefficient reliability (mean [range]):",
      paste(sprintf("%s %s = %.3f [%.3f, %.3f]", rel$Group, rel$Term, rel$Mean_Reliability, rel$Min_Reliability, rel$Max_Reliability), collapse = "; "),
      "."
    )
  } else {
    ""
  }
  apa_html_table(
    r2_disp,
    table_number = table_number,
    title = title,
    note = html_escape(trimws(paste(
      "Marginal R2 reflects variance explained by fixed effects alone; Conditional R2 reflects fixed plus random effects (Nakagawa, Johnson, & Schielzeth, 2017).",
      rel_note
    )))
  )
}

apa_reliability_html <- function(result, table_number = "5b", title = "Empirical Bayes Random-Coefficient Reliability Estimates") {
  rel <- mlm_reliability(result)
  if (!nrow(rel)) {
    return(apa_html_table(
      data.frame(`Grouping Level` = character(), Coefficient = character(), Reliability = character(), check.names = FALSE),
      table_number = table_number,
      title = title,
      note = "Empirical Bayes random-coefficient reliabilities are computed for Gaussian mixed models."
    ))
  }
  disp <- data.frame(
    `Grouping Level` = rel$Group,
    `Random Coefficient` = rel$Term,
    Clusters = as.character(rel$Units),
    Variance = sprintf("%.3f", rel$Variance),
    `Mean Reliability` = sprintf("%.3f", rel$Mean_Reliability),
    `Cluster Range` = sprintf("[%.3f, %.3f]", rel$Min_Reliability, rel$Max_Reliability),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  apa_html_table(
    disp,
    table_number = table_number,
    title = title,
    note = "Reliability lambda estimates the ratio of parameter variance to total (parameter + sampling) variance across clusters (Raudenbush & Bryk, 2002, Eq. 3.45)."
  )
}

apa_software_html <- function(packages = mlm_default_packages(), table_number = 6, title = "Software and R Packages Used in the Analysis") {
  apa_html_table(
    mlm_software_table(packages),
    table_number = table_number,
    title = title,
    note = "Package versions should be reported with manuscripts or supplementary materials so analyses can be reproduced in the same computational environment."
  )
}

apa_tables_html_document <- function(result, exponentiate = FALSE) {
  pkgs <- active_packages_for_result(result)
  paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>mlmr APA Tables</title>",
    "<style>",
    "body{font-family:'Times New Roman',serif;max-width:980px;margin:32px auto;line-height:1.35;color:#111;}",
    ".apa-table-wrap{margin-bottom:28px;}.apa-table-number{font-weight:bold;margin-bottom:4px;}",
    ".apa-table-title{font-style:italic;margin-bottom:8px;}.apa-table{width:100%;border-collapse:collapse;border-top:1.5px solid currentColor;border-bottom:1.5px solid currentColor;}",
    ".apa-table thead{border-bottom:1px solid currentColor;}.apa-table th,.apa-table td{padding:6px 8px;border:none;vertical-align:top;text-align:right;}",
    ".apa-table th:first-child,.apa-table td:first-child{text-align:left;}.apa-table th{font-weight:normal;}.apa-table-note{margin-top:8px;}",
    "</style></head><body>",
    apa_fixed_html(result, exponentiate = exponentiate),
    apa_dummy_html(result$data, names(result$spec$fixed)),
    apa_variance_html(result$fit),
    apa_icc_html(result$fit),
    apa_r2_html(result),
    apa_software_html(packages = pkgs),
    "</body></html>"
  )
}

apa_tables_latex_document <- function(result, exponentiate = FALSE) {
  pkgs <- active_packages_for_result(result)
  paste(
    "% mlmr APA manuscript tables",
    "% Recommended preamble: \\usepackage{amsmath} \\usepackage{graphicx}",
    paste(apa_fixed_latex(result, exponentiate = exponentiate), collapse = "\n"),
    "",
    paste(apa_dummy_latex(result$data, names(result$spec$fixed)), collapse = "\n"),
    "",
    paste(apa_variance_latex(result$fit), collapse = "\n"),
    "",
    paste(apa_icc_latex(result$fit), collapse = "\n"),
    "",
    paste(apa_software_latex(packages = pkgs), collapse = "\n"),
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
  pkgs <- active_packages_for_result(result)
  list(
    fixed_effects = apa_fixed_table(result, exponentiate = exponentiate),
    dummy_coding = dummy_coding_table(result$data, names(result$spec$fixed)),
    variance_components = variance_table(result$fit),
    icc = icc_table(result$fit),
    software = mlm_software_table(packages = pkgs)
  )
}

raw_latex_bundle <- function(result) {
  eq <- mlm_latex_equations(result)
  pkgs <- active_packages_for_result(result)
  paste(
    "% Recommended preamble for equations, matrices, and resized tables:",
    "% \\usepackage{amsmath}",
    "% \\usepackage{graphicx}",
    "",
    "% Table 1. Fixed effects",
    paste(apa_fixed_latex(result, exponentiate = TRUE), collapse = "\n"),
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
    paste(apa_software_latex(packages = pkgs), collapse = "\n"),
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

model_writeup <- function(result) {
  fit <- result$fit
  spec <- result$spec
  fixed <- apa_fixed_table(result, exponentiate = TRUE)
  icc <- icc_table(fit)
  r2 <- tryCatch(mlm_r2(result), error = function(e) NULL)
  diag <- mlm_diagnostics(fit)
  formula_text <- paste(deparse(result$formula, width.cutoff = 500), collapse = " ")
  outcome <- spec$outcome
  groups <- names(spec$random)
  random_slopes <- unique(unlist(lapply(spec$random, function(x) x$slopes %||% character())))
  sig <- fixed[!fixed$Predictor %in% "(Intercept)" & is_significant_p_string(fixed$p), , drop = FALSE]
  sig_text <- if (nrow(sig)) {
    p_formatted <- format_apa_p_prose(sig$p)
    paste(sprintf("%s (b = %s, SE = %s, %s, 95%% CI %s)", sig$Predictor, sig$b, sig$SE, p_formatted, sig$`95% CI`), collapse = "; ")
  } else {
    "No fixed effects met the conventional alpha = .05 threshold in the fitted model"
  }
  icc_text <- if (nrow(icc)) {
    paste0(paste(sprintf("%s ICC = %.3f", icc$group, icc$icc), collapse = "; "), ".")
  } else {
    icc_note(fit)
  }
  r2_sentence <- if (!is.null(r2) && is.finite(r2$R2_marginal)) {
    sprintf(" Overall variance explained was marginal R^2 = %.3f and conditional R^2 = %.3f.", r2$R2_marginal, r2$R2_conditional)
  } else {
    ""
  }
  model_text <- if (is_gaussian_fit(fit)) {
    sprintf(
      "A Gaussian multilevel linear model was estimated for %s using %s. The fitted model was specified as: %s.",
      outcome,
      if (isTRUE(lme4::isREML(fit))) "restricted maximum likelihood" else "maximum likelihood",
      formula_text
    )
  } else {
    sprintf(
      "A %s generalized mixed-effects model with a %s link was estimated for %s using maximum likelihood. The fitted model was specified as: %s.",
      spec$distribution, spec$link, outcome, formula_text
    )
  }
  paste(
    model_text,
    sprintf(
      "The model included fixed effects for %s and random effects for %s. Random slope variation was estimated for %s, allowing the corresponding lower-level associations to vary across higher-level units.",
      if (length(spec$fixed)) paste(names(spec$fixed), collapse = ", ") else "the intercept only",
      paste(groups, collapse = ", "),
      if (length(random_slopes)) paste(random_slopes, collapse = ", ") else "no predictors"
    ),
    sprintf("The fixed-effect results indicated the following statistically detectable associations: %s. %s", sig_text, fixed_effects_note(result)),
    sprintf("Variance partitioning suggested the following clustering structure: %s%s", icc_text, r2_sentence),
    sprintf(
      "Model diagnostics reported: %s. Singularity or convergence warnings should be interpreted as evidence that the specified random-effects structure may be too complex for the available data or may require alternative centering, simplification, or optimizer settings.",
      paste(sprintf("%s = %s", diag$check, diag$value), collapse = "; ")
    ),
    "Substantively, the model separates within-unit predictors from higher-level contextual predictors while estimating heterogeneity in selected slopes. This is the central advantage of the multilevel specification over a single-level regression: it permits the average association, the between-group variance, and the group-to-group variation in key effects to be evaluated within one coherent model.",
    sep = "\n\n"
  )
}

mlm_reproducibility_check <- function(result, REML = NULL, optimizer = NULL, maxfun = NULL) {
  settings <- result_settings(result, REML, optimizer, maxfun)
  REML <- settings$REML
  optimizer <- settings$optimizer
  maxfun <- settings$maxfun
  df_method <- settings$df_method
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
  pkgs <- active_packages_for_result(result)
  sw_tab <- mlm_software_table(packages = pkgs)
  has_ddf <- is_gaussian_fit(result$fit) && df_method %in% c("satterthwaite", "kenward-roger")
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
    add("OK", "Estimator settings captured", sprintf("Distribution: %s; link: %s; REML: %s; optimizer: %s; maxfun: %s; df_method: %s.", spec$distribution, spec$link, REML, optimizer, maxfun, df_method), "Use ML rather than REML for nested fixed-effect comparisons."),
    add(if (length(centering) && any(centering != "none")) "OK" else "Review", "Centering decisions captured", if (length(centering)) paste(sprintf("%s = %s", names(centering), centering), collapse = "; ") else "No fixed predictors selected.", "Confirm centering choices match within-, between-, or contextual-effect claims."),
    add("OK", "Random-effects structure captured", sprintf("Groups: %s; random slopes: %s.", paste(names(spec$random), collapse = ", "), if (length(random_slopes)) paste(random_slopes, collapse = ", ") else "none"), "Report simplification decisions if convergence or singularity occurs."),
    add(if (identical(singular, "TRUE")) "Review" else "OK", "Singular-fit check", singular, "If TRUE, consider a simpler random-effects structure or explain why the boundary estimate is acceptable."),
    add(if (length(convergence) && !identical(convergence, "None reported")) "Review" else "OK", "Convergence messages", convergence, "Resolve convergence warnings before treating the model as final."),
    add(if (has_ddf) "OK" else "Review", "Inference labels reviewed", fixed_effects_note(result), "Use denominator-df, bootstrap, profile-likelihood, or journal-preferred methods when confirmatory inference requires them."),
    add(if (is_gaussian_fit(result$fit)) "OK" else "Review", "ICC reporting reviewed", icc_note(result$fit), "For GLMMs, report a family-specific ICC only after selecting and documenting an appropriate scale."),
    add("OK", "Tables and equations generated", "APA tables, variance components, R^2, equations, LaTeX, and Quarto/R exports are available. Classical ICCs are produced for Gaussian mixed models only.", "Check labels, reference categories, notation, and journal formatting manually."),
    add("OK", "Software versions captured", paste(sprintf("%s %s", sw_tab$Software, sw_tab$Version), collapse = "; "), "Include the software table or APA software statement in supplementary materials.")
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
  pkgs <- active_packages_for_result(result)
  paste(
    sprintf("We estimated a %s for %s using %s in lme4. The fitted formula was %s.", distribution_text, spec$outcome, estimator, formula_text),
    sprintf(
      "The model included fixed effects for %s and random-effect blocks for %s. Random slopes were estimated for %s.",
      if (length(fixed_terms)) paste(fixed_terms, collapse = ", ") else "the intercept only",
      if (length(groups)) paste(groups, collapse = ", ") else "no grouping factors",
      if (length(random_slopes)) paste(random_slopes, collapse = ", ") else "no predictors"
    ),
    sprintf(
      "Centering choices were recorded as follows: %s.",
      if (length(centered_terms)) paste(sprintf("%s was %s centered", names(centered_terms), centered_terms), collapse = "; ") else "no selected predictors were centered"
    ),
    fixed_effects_note(result),
    icc_note(result$fit),
    sprintf("The analysis used optimizer %s with a maximum of %s function evaluations. Reproducible R code, model diagnostics, APA-style tables, equations, LaTeX output, and software version information were exported from mlmr.", optimizer, maxfun),
    mlm_software_apa(packages = pkgs),
    sep = "\n\n"
  )
}

manuscript_report_markdown <- function(result, REML = NULL, optimizer = NULL, maxfun = NULL) {
  settings <- result_settings(result, REML, optimizer, maxfun)
  REML <- settings$REML
  optimizer <- settings$optimizer
  maxfun <- settings$maxfun
  pkgs <- active_packages_for_result(result)
  summary <- model_summary_cards(result, optimizer)
  diagnostics <- mlm_diagnostics(result$fit)
  fixed <- apa_fixed_table(result, exponentiate = TRUE)
  variance <- apa_variance_formatted_table(result$fit)
  icc <- icc_table(result$fit)
  r2 <- mlm_r2(result)
  rel <- mlm_reliability(result)
  software <- mlm_software_table(packages = pkgs)
  reproducibility <- mlm_reproducibility_check(result, REML = REML, optimizer = optimizer, maxfun = maxfun)
  eq <- mlm_latex_equations(result)
  c(
    "---",
    "title: \"mlmr Multilevel Model Report\"",
    "format: html",
    "---",
    "",
    "# Model Summary",
    "",
    md_pipe_table(summary),
    "",
    "# Publication Readiness",
    "",
    "## Methods Text",
    "",
    mlm_methods_text(result, REML = REML, optimizer = optimizer, maxfun = maxfun),
    "",
    "## Reproducibility Checklist",
    "",
    md_pipe_table(reproducibility),
    "",
    "# Results Write-Up",
    "",
    model_writeup(result),
    "",
    "# Fixed Effects",
    "",
    md_pipe_table(fixed),
    "",
    "## Fixed Effects With Cluster-Robust Standard Errors (CR1)",
    "",
    md_pipe_table(tryCatch(mlm_robust_se(result), error = function(e) data.frame())),
    "",
    "# Variance Components",
    "",
    md_pipe_table(variance),
    "",
    "# Intraclass Correlations",
    "",
    md_pipe_table(icc),
    "",
    "# Model R-Squared and Reliability",
    "",
    md_pipe_table(r2),
    "",
    if (nrow(rel)) c("## Random-Coefficient Reliability", "", md_pipe_table(rel), "") else character(),
    "# Software and Packages",
    "",
    mlm_software_apa(packages = pkgs),
    "",
    md_pipe_table(software),
    "",
    "## Optional papaja Citation Workflow",
    "",
    "If `papaja` is installed, the following code creates APA-style R/package citations and a BibTeX file for the packages used in this analysis.",
    "",
    "```r",
    mlm_papaja_code(packages = pkgs),
    "```",
    "",
    "# Equations",
    "",
    "## Level-by-Level Equations",
    "",
    paste(vapply(eq$equations, quarto_math_block, character(1)), collapse = "\n\n"),
    "",
    "## Combined Full Equation",
    "",
    quarto_math_block(eq$combined),
    "",
    "## Tau Structures",
    "",
    paste(sprintf("$$\n%s\n$$", eq$tau), collapse = "\n\n"),
    "",
    "## Coefficient Key",
    "",
    eq$note,
    "",
    md_pipe_table(equation_key_table(result)),
    "",
    "# Diagnostics",
    "",
    md_pipe_table(diagnostics),
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

mlm_publication_bundle <- function(result, REML = NULL, optimizer = NULL, maxfun = NULL) {
  pkgs <- active_packages_for_result(result)
  list(
    methods_text = mlm_methods_text(result, REML = REML, optimizer = optimizer, maxfun = maxfun),
    reproducibility_check = mlm_reproducibility_check(result, REML = REML, optimizer = optimizer, maxfun = maxfun),
    software_statement = mlm_software_apa(packages = pkgs),
    formula = paste(deparse(result$formula, width.cutoff = 500), collapse = " "),
    apa_tables = mlm_apa_tables(result, exponentiate = TRUE),
    equations = mlm_latex_equations(result),
    reproducible_code = generate_repro_code(result, REML = REML, optimizer = optimizer, maxfun = maxfun)
  )
}

mlm_write_publication_bundle <- function(result, file, REML = NULL, optimizer = NULL, maxfun = NULL) {
  bundle <- mlm_publication_bundle(result, REML = REML, optimizer = optimizer, maxfun = maxfun)
  pkgs <- active_packages_for_result(result)
  stage_dir <- tempfile("mlmr_bundle_")
  bundle_root <- file.path(stage_dir, "mlmr_publication_bundle")
  dir.create(bundle_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(stage_dir, recursive = TRUE), add = TRUE)

  readme_lines <- c(
    "# mlmr Publication & Reproducibility Bundle",
    "",
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "## Contents",
    "",
    "- `mlmr_model_report.qmd`: Quarto report with methods, results, APA tables, equations, and diagnostics.",
    "- `mlmr_reproducible_analysis.R`: Standalone R script that reproduces centering, estimation, and manuscript tables.",
    "- `mlmr_tables_and_equations.tex`: Raw LaTeX tables, level-by-level equations, combined equation, and Tau matrices.",
    "- `mlmr_apa_tables.html`: Standalone APA-styled HTML tables (also opens cleanly in Word).",
    "- `reproducibility_checklist.csv`: Reviewer-facing reproducibility audit.",
    "- `software_versions.csv`: Exact R and package versions recorded for the session.",
    "- `level1_residuals.csv`: Complete-case Level-1 observed values, fitted values, and residuals.",
    "- `cluster_eb_residuals.csv`: Cluster-level empirical Bayes random effects and posterior standard errors.",
    "",
    "## Methods Text",
    "",
    bundle$methods_text,
    "",
    "## Reproducibility Checklist",
    "",
    md_pipe_table(bundle$reproducibility_check),
    "",
    "## Software Statement",
    "",
    bundle$software_statement
  )

  res_files <- mlm_residual_files(result)
  writeLines(readme_lines, file.path(bundle_root, "README_bundle.md"), useBytes = TRUE)
  writeLines(manuscript_report_markdown(result, REML, optimizer, maxfun), file.path(bundle_root, "mlmr_model_report.qmd"), useBytes = TRUE)
  writeLines(paste(bundle$reproducible_code, collapse = "\n"), file.path(bundle_root, "mlmr_reproducible_analysis.R"), useBytes = TRUE)
  writeLines(raw_latex_bundle(result), file.path(bundle_root, "mlmr_tables_and_equations.tex"), useBytes = TRUE)
  writeLines(apa_tables_html_document(result, exponentiate = TRUE), file.path(bundle_root, "mlmr_apa_tables.html"), useBytes = TRUE)
  utils::write.csv(bundle$reproducibility_check, file.path(bundle_root, "reproducibility_checklist.csv"), row.names = FALSE)
  utils::write.csv(mlm_software_table(packages = pkgs), file.path(bundle_root, "software_versions.csv"), row.names = FALSE)
  utils::write.csv(res_files$level1, file.path(bundle_root, "level1_residuals.csv"), row.names = FALSE)
  utils::write.csv(res_files$cluster, file.path(bundle_root, "cluster_eb_residuals.csv"), row.names = FALSE)

  abs_target <- normalizePath(dirname(file), winslash = "/", mustWork = TRUE)
  target_zip <- file.path(abs_target, basename(file))
  if (file.exists(target_zip)) unlink(target_zip)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(stage_dir)
  status <- utils::zip(zipfile = target_zip, files = "mlmr_publication_bundle", flags = "-r9Xq")
  if (!identical(status, 0L) || !file.exists(target_zip)) {
    stop("Failed to create publication bundle archive.", call. = FALSE)
  }
  invisible(target_zip)
}
