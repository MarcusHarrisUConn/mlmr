# Lightweight end-to-end smoke test of the exported mlmr API.
# Detailed regression tests run in tests/testthat/.
library(mlmr)

stopifnot(inherits(tryCatch(example_hsb(n_schools = 1), error = identity), "error"))

dat <- example_hsb(n_schools = 6, min_students = 8, max_students = 10, seed = 101)
dat_long <- example_longitudinal(n_students = 12, n_waves = 4, seed = 101)
stopifnot(nrow(dat_long) == 48L)

spec <- mlm_spec(
  outcome = "mathscore",
  fixed = list(
    ses = list(center = "CWC"),
    meanses = list(center = "GMC"),
    sector = list(center = "none")
  ),
  grouping = list(schoolid = "schoolid"),
  random = list(schoolid = list(intercept = TRUE, slopes = "ses", correlation = TRUE)),
  interactions = list(c("ses", "sector")),
  data = dat
)

formula_text <- paste(deparse(mlm_formula(spec), width.cutoff = 500), collapse = " ")
stopifnot(grepl("mathscore ~ ses_CWC + meanses_GMC + sector + ses_CWC:sector + (1 + ses_CWC | schoolid)", formula_text, fixed = TRUE))

pre_eq <- mlm_spec_equations(spec)
stopifnot(length(pre_eq$equations) == 2L)

result <- suppressWarnings(mlm_fit(spec, REML = TRUE, optimizer = "bobyqa", maxfun = 10000))
stopifnot(inherits(result, "mlm_result"))

r2 <- mlm_r2(result)
rel <- mlm_reliability(result)
rob <- mlm_robust_se(result)
vt <- mlm_variance_tests(result)
ht <- mlm_homogeneity_test(result)
stopifnot(is.data.frame(r2) && nrow(r2) == 1L)
stopifnot(is.data.frame(rel) && nrow(rel) == 2L && identical(rel$Term, c("(Intercept)", "ses_CWC")))
stopifnot(is.data.frame(rob) && nrow(rob) == length(lme4::fixef(result$fit)))
stopifnot(is.data.frame(vt) && nrow(vt) == 2L)
stopifnot(is.data.frame(ht) && nrow(ht) == 1L)

tables <- mlm_apa_tables(result)
stopifnot(all(c("fixed_effects", "dummy_coding", "variance_components", "icc", "software") %in% names(tables)))

eq <- mlm_latex_equations(result)
stopifnot(length(eq$equations) == 2L && "Role" %in% names(eq$key))
stopifnot(identical(pre_eq$equations, eq$equations))

parity <- mlm_hlm8_parity()
stopifnot(nrow(parity) >= 24L)

bundle <- mlm_publication_bundle(result)
stopifnot(all(c("methods_text", "reproducibility_check", "software_statement", "formula", "apa_tables", "equations", "reproducible_code") %in% names(bundle)))

benchmarks <- mlm_validation_benchmarks(run = FALSE)
stopifnot(nrow(benchmarks) == 8L && all(benchmarks$`Formula Match`))
