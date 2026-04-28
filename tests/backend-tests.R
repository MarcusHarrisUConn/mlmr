get_mlmr <- function(name) getFromNamespace(name, "mlmr")

example_hsb <- get_mlmr("example_hsb")
mlm_spec <- get_mlmr("mlm_spec")
build_formula <- get_mlmr("build_formula")
center_predictors <- get_mlmr("center_predictors")
mlm_fit <- get_mlmr("mlm_fit")
mlm_latex_equations <- get_mlmr("mlm_latex_equations")
tau_label_table <- get_mlmr("tau_label_table")
model_readiness_table <- get_mlmr("model_readiness_table")
model_readiness_has_stops <- get_mlmr("model_readiness_has_stops")
apa_fixed_table <- get_mlmr("apa_fixed_table")
apa_tables_latex_document <- get_mlmr("apa_tables_latex_document")
apa_tables_html_document <- get_mlmr("apa_tables_html_document")

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

formula_text <- paste(deparse(build_formula(spec), width.cutoff = 500), collapse = " ")
stopifnot(grepl("mathscore", formula_text, fixed = TRUE))
stopifnot(grepl("ses_CWC", formula_text, fixed = TRUE))
stopifnot(grepl("meanses_GMC", formula_text, fixed = TRUE))
stopifnot(grepl("ses_CWC:sector", formula_text, fixed = TRUE))
stopifnot(grepl("(1 + ses_CWC | schoolid)", formula_text, fixed = TRUE))

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
