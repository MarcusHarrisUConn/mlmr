get_mlmr <- function(name) getFromNamespace(name, "mlmr")

example_hsb <- get_mlmr("example_hsb")
mlm_spec <- get_mlmr("mlm_spec")
build_formula <- get_mlmr("build_formula")
center_predictors <- get_mlmr("center_predictors")
mlm_fit <- get_mlmr("mlm_fit")
mlm_formula <- get_mlmr("mlm_formula")
mlm_latex_equations <- get_mlmr("mlm_latex_equations")
mlm_apa_tables <- get_mlmr("mlm_apa_tables")
mlm_software_table <- get_mlmr("mlm_software_table")
mlm_software_apa <- get_mlmr("mlm_software_apa")
mlm_papaja_code <- get_mlmr("mlm_papaja_code")
mlm_supported_models <- get_mlmr("mlm_supported_models")
tau_label_table <- get_mlmr("tau_label_table")
model_readiness_table <- get_mlmr("model_readiness_table")
model_readiness_has_stops <- get_mlmr("model_readiness_has_stops")
apa_fixed_table <- get_mlmr("apa_fixed_table")
apa_tables_latex_document <- get_mlmr("apa_tables_latex_document")
apa_tables_html_document <- get_mlmr("apa_tables_html_document")
raw_latex_bundle <- get_mlmr("raw_latex_bundle")
latex_wrapped_equation <- get_mlmr("latex_wrapped_equation")
manuscript_report_markdown <- get_mlmr("manuscript_report_markdown")
generate_repro_code <- get_mlmr("generate_repro_code")

stopifnot(inherits(tryCatch(example_hsb(n_schools = 1), error = identity), "error"))

dat <- example_hsb(n_schools = 8, min_students = 8, max_students = 10, seed = 101)

spec <- mlm_spec(
  outcome = "mathscore",
  fixed = list(
    ses = list(center = "CWC"),
    meanses = list(center = "GMC"),
    sector = list(center = "none")
  ),
  grouping = list(schoolid = "schoolid"),
  random = list(
    schoolid = list(intercept = TRUE, slopes = c("ses"), correlation = TRUE)
  ),
  interactions = list(c("ses", "sector")),
  data = dat
)

missing_var_error <- tryCatch(
  mlm_spec(
    outcome = "mathscore",
    fixed = list(not_in_data = list(center = "none")),
    grouping = list(schoolid = "schoolid"),
    random = list(schoolid = list(intercept = TRUE, slopes = character(), correlation = TRUE)),
    data = dat
  ),
  error = identity
)
stopifnot(inherits(missing_var_error, "error"))
stopifnot(grepl("missing from `data`", conditionMessage(missing_var_error), fixed = TRUE))

formula_text <- paste(deparse(build_formula(spec), width.cutoff = 500), collapse = " ")
formula_text_public <- paste(deparse(mlm_formula(spec), width.cutoff = 500), collapse = " ")
stopifnot(identical(formula_text, formula_text_public))
stopifnot(grepl("mathscore", formula_text, fixed = TRUE))
stopifnot(grepl("ses_CWC", formula_text, fixed = TRUE))
stopifnot(grepl("meanses_GMC", formula_text, fixed = TRUE))
stopifnot(grepl("ses_CWC:sector", formula_text, fixed = TRUE))
stopifnot(grepl("(1 + ses_CWC | schoolid)", formula_text, fixed = TRUE))

uncorrelated_spec <- spec
uncorrelated_spec$random$schoolid$correlation <- FALSE
uncorrelated_formula <- paste(deparse(build_formula(uncorrelated_spec), width.cutoff = 500), collapse = " ")
stopifnot(grepl("(1 | schoolid)", uncorrelated_formula, fixed = TRUE))
stopifnot(grepl("(0 + ses_CWC | schoolid)", uncorrelated_formula, fixed = TRUE))

three_level_spec <- mlm_spec(
  outcome = "mathscore",
  fixed = list(ses = list(center = "CWC"), meanses = list(center = "GMC")),
  grouping = list(schoolid = "schoolid", districtid = "districtid"),
  random = list(
    schoolid = list(intercept = TRUE, slopes = "ses", correlation = TRUE),
    districtid = list(intercept = TRUE, slopes = character(), correlation = TRUE)
  ),
  predictor_levels = list(level1 = "ses", level2 = "meanses"),
  data = dat
)
three_level_formula <- paste(deparse(build_formula(three_level_spec), width.cutoff = 500), collapse = " ")
stopifnot(grepl("(1 + ses_CWC | schoolid)", three_level_formula, fixed = TRUE))
stopifnot(grepl("(1 | districtid)", three_level_formula, fixed = TRUE))

centered <- center_predictors(dat, spec$fixed, spec$grouping)
stopifnot("ses_CWC" %in% names(centered$data))
stopifnot("meanses_GMC" %in% names(centered$data))
school_means <- tapply(centered$data$ses_CWC, centered$data$schoolid, mean)
stopifnot(max(abs(school_means), na.rm = TRUE) < 1e-10)
stopifnot(abs(mean(centered$data$meanses_GMC, na.rm = TRUE)) < 1e-10)

fit <- mlm_fit(spec, REML = TRUE, optimizer = "bobyqa", maxfun = 10000)
eq <- mlm_latex_equations(fit)
stopifnot(length(eq$equations) >= 2)
stopifnot(grepl("\\beta_{0j}", eq$equations[[1]], fixed = TRUE))
stopifnot(grepl("\\gamma_{00}", eq$combined, fixed = TRUE))
stopifnot(length(eq$tau) >= 1)

wrapped_eq <- latex_wrapped_equation(eq$combined, terms_per_line = 2)
stopifnot(grepl("\\begin{array}{rcl}", wrapped_eq, fixed = TRUE))
stopifnot(!grepl("\\begin{aligned}", wrapped_eq, fixed = TRUE))

tau_labels <- tau_label_table(fit)
stopifnot(nrow(tau_labels) >= 1)
stopifnot(all(c("Group", "Matrix Index", "Coefficient", "Estimated") %in% names(tau_labels)))

readiness <- model_readiness_table(dat, spec)
stopifnot(nrow(readiness) >= 1)
stopifnot(!model_readiness_has_stops(readiness))

bad_spec <- spec
bad_spec$outcome <- "schoolid"
bad_readiness <- model_readiness_table(dat, bad_spec)
stopifnot(model_readiness_has_stops(bad_readiness))

fixed_tab <- apa_fixed_table(fit$fit)
stopifnot(nrow(fixed_tab) >= 1)
stopifnot(all(c("Predictor", "b", "SE", "p", "95% CI") %in% names(fixed_tab)))

latex_tables <- apa_tables_latex_document(fit)
stopifnot(grepl("\\begin{table}", latex_tables, fixed = TRUE))

html_tables <- apa_tables_html_document(fit)
stopifnot(grepl("<html>", html_tables, fixed = TRUE))

tables_list <- mlm_apa_tables(fit)
stopifnot(all(c("fixed_effects", "dummy_coding", "variance_components", "icc", "software") %in% names(tables_list)))

tables_latex_public <- mlm_apa_tables(fit, format = "latex")
stopifnot(grepl("Software and R packages", tables_latex_public, fixed = TRUE))
stopifnot(grepl("\\resizebox{\\textwidth}{!}", tables_latex_public, fixed = TRUE))

raw_latex <- raw_latex_bundle(fit)
stopifnot(grepl("% Table 1. Fixed effects", raw_latex, fixed = TRUE))
stopifnot(grepl("% Table 5. Software and R packages", raw_latex, fixed = TRUE))
stopifnot(grepl("% Combined full equation", raw_latex, fixed = TRUE))
stopifnot(grepl("% Tau variance-covariance structures", raw_latex, fixed = TRUE))

report_markdown <- paste(manuscript_report_markdown(fit), collapse = "\n")
stopifnot(grepl("mlmr Multilevel Model Report", report_markdown, fixed = TRUE))
stopifnot(grepl("Optional papaja Citation Workflow", report_markdown, fixed = TRUE))
stopifnot(grepl("Raw LaTeX", report_markdown, fixed = TRUE))

repro_code <- paste(generate_repro_code(fit, REML = TRUE, optimizer = "bobyqa", maxfun = 10000), collapse = "\n")
stopifnot(grepl("Table 1: APA Fixed Effects", repro_code, fixed = TRUE))
stopifnot(grepl("Table 5: Software and R Packages", repro_code, fixed = TRUE))
stopifnot(grepl("Raw LaTeX Equations", repro_code, fixed = TRUE))

software_table <- mlm_software_table()
stopifnot(all(c("Software", "Version", "Purpose", "Citation") %in% names(software_table)))
stopifnot("R" %in% software_table$Software)
stopifnot("lme4" %in% software_table$Software)

software_text <- mlm_software_apa()
stopifnot(grepl("Analyses were conducted in R version", software_text, fixed = TRUE))
stopifnot(grepl("lme4 version", software_text, fixed = TRUE))

papaja_code <- paste(mlm_papaja_code(), collapse = "\n")
stopifnot(grepl("papaja::r_refs", papaja_code, fixed = TRUE))
stopifnot(grepl("papaja::cite_r", papaja_code, fixed = TRUE))
stopifnot(grepl("omit = FALSE", papaja_code, fixed = TRUE))

binomial_spec <- mlm_spec(
  outcome = "passmath",
  distribution = "binomial",
  link = "logit",
  fixed = list(ses = list(center = "CWC")),
  grouping = list(schoolid = "schoolid"),
  random = list(schoolid = list(intercept = TRUE, slopes = character(), correlation = TRUE)),
  data = dat
)
binomial_fit <- mlm_fit(binomial_spec, optimizer = "bobyqa", maxfun = 10000)
stopifnot(inherits(binomial_fit$fit, "glmerMod"))
binomial_diagnostics <- get_mlmr("mlm_diagnostics")(binomial_fit$fit)
stopifnot("Overdispersion ratio" %in% binomial_diagnostics$check)

scope <- mlm_supported_models()
stopifnot(nrow(scope) >= 10)
stopifnot(all(c("Area", "Status", "Scope", "User_responsibility") %in% names(scope)))
stopifnot("Nested structures" %in% scope$Area)
stopifnot("Multiple membership" %in% scope$Area)
stopifnot(any(scope$Status == "Experimental"))
stopifnot(any(scope$Status == "Planned"))

if (file.exists("R/mlm_core.R") && file.exists("inst/app/R/mlm_core.R")) {
  root_core <- readLines("R/mlm_core.R", warn = FALSE)
  app_core <- readLines("inst/app/R/mlm_core.R", warn = FALSE)
  stopifnot(identical(root_core, app_core))
}

if (file.exists("app.R") && file.exists("inst/app/app.R")) {
  root_app <- readLines("app.R", warn = FALSE)
  installed_app <- readLines("inst/app/app.R", warn = FALSE)
  stopifnot(identical(root_app, installed_app))
}

if (file.exists("www/style.css") && file.exists("inst/app/www/style.css")) {
  root_css <- readLines("www/style.css", warn = FALSE)
  installed_css <- readLines("inst/app/www/style.css", warn = FALSE)
  stopifnot(identical(root_css, installed_css))
}
