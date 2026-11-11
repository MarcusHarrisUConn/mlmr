test_that("exported reproducible code reproduces the fitted model", {
  result <- quiet_fit(two_level_spec(), REML = FALSE)
  env <- run_repro_code(result, hsb)
  expect_equal(stats::AIC(env$fit), stats::AIC(result$fit), tolerance = 1e-8)
  expect_equal(lme4::fixef(env$fit), lme4::fixef(result$fit), tolerance = 1e-8)
  expect_false(lme4::isREML(env$fit))
})

test_that("reproducible code matches the model when data are incomplete", {
  dat <- hsb
  set.seed(7)
  dat$mathscore[sample(nrow(dat), 30)] <- NA
  dat$ses[sample(nrow(dat), 25)] <- NA
  result <- quiet_fit(two_level_spec(data = dat))
  env <- run_repro_code(result, dat)
  expect_identical(stats::nobs(env$fit), stats::nobs(result$fit))
  expect_equal(lme4::fixef(env$fit), lme4::fixef(result$fit), tolerance = 1e-8)
})

test_that("reproducible code runs for generalized models", {
  spec <- mlm_spec(
    "absences", "poisson", "log",
    fixed = list(ses = list(center = "GMC")),
    grouping = list(schoolid = "schoolid"),
    random = school_random(),
    data = hsb
  )
  result <- quiet_fit(spec)
  env <- run_repro_code(result, hsb)
  expect_equal(lme4::fixef(env$fit), lme4::fixef(result$fit), tolerance = 1e-6)
})

test_that("reporting text follows the fitted model, not argument defaults", {
  result <- quiet_fit(two_level_spec(), REML = FALSE, maxfun = 12345)
  expect_identical(result$settings$REML, FALSE)
  text <- mlm_methods_text(result)
  expect_match(text, "using maximum likelihood in lme4", fixed = TRUE)
  expect_match(text, "maximum of 12345 function evaluations", fixed = TRUE)
  # A stale argument cannot contradict the fit.
  expect_match(mlm_methods_text(result, REML = TRUE), "using maximum likelihood in lme4", fixed = TRUE)
  check <- mlm_reproducibility_check(result)
  expect_match(check$Evidence[check$Item == "Estimator settings captured"], "REML: FALSE", fixed = TRUE)
  bundle <- mlm_publication_bundle(result)
  expect_true(any(grepl("REML = FALSE", bundle$reproducible_code, fixed = TRUE)))
  expect_named(bundle, c("methods_text", "reproducibility_check", "software_statement", "formula", "apa_tables", "equations", "reproducible_code"))
})

test_that("fixed-effects tables can report exponentiated coefficients", {
  logit <- quiet_fit(mlm_spec(
    "passmath", "binomial", "logit",
    fixed = list(ses = list(center = "GMC")),
    grouping = list(schoolid = "schoolid"), random = school_random(), data = hsb
  ))
  plain <- mlm_apa_tables(logit)$fixed_effects
  expect_named(plain, c("Predictor", "b", "SE", "Statistic", "p", "95% CI"))
  odds <- mlm_apa_tables(logit, exponentiate = TRUE)$fixed_effects
  expect_named(odds, c("Predictor", "b", "SE", "Statistic", "p", "95% CI", "OR", "OR 95% CI"))
  expect_identical(odds$OR, sprintf("%.2f", exp(lme4::fixef(logit$fit))))
  expect_match(mlm_apa_tables(logit, "latex", exponentiate = TRUE), "OR 95\\% CI", fixed = TRUE)
  expect_match(mlm_apa_tables(logit, "html", exponentiate = TRUE), "exponentiated coefficient", fixed = TRUE)

  counts <- quiet_fit(mlm_spec(
    "absences", "poisson", "log",
    fixed = list(ses = list(center = "GMC")),
    grouping = list(schoolid = "schoolid"), random = school_random(), data = hsb
  ))
  expect_true("IRR" %in% names(mlm_apa_tables(counts, exponentiate = TRUE)$fixed_effects))

  gaussian <- quiet_fit(two_level_spec())
  expect_named(mlm_apa_tables(gaussian, exponentiate = TRUE)$fixed_effects, names(plain))
})

test_that("ICCs are reported for Gaussian models only", {
  result <- quiet_fit(mlm_spec("mathscore", grouping = list(schoolid = "schoolid"), random = school_random(), data = hsb))
  icc <- mlm_apa_tables(result)$icc
  vc <- as.data.frame(lme4::VarCorr(result$fit))
  expect_equal(icc$icc, round(vc$vcov[[1]] / sum(vc$vcov), 4))
  logit <- quiet_fit(mlm_spec(
    "passmath", "binomial", "logit",
    grouping = list(schoolid = "schoolid"), random = school_random(), data = hsb
  ))
  expect_identical(nrow(mlm_apa_tables(logit)$icc), 0L)
})

test_that("LaTeX table cells escape special characters", {
  expect_identical(
    mlmr:::latex_table_escape("a_b 50% <x> {y} $5 #1 c:\\d & e"),
    "a\\_b 50\\% $<$x$>$ \\{y\\} \\$5 \\#1 c:\\textbackslash{}d \\& e"
  )
})

test_that("manuscript report includes the coefficient key", {
  report <- paste(mlmr:::manuscript_report_markdown(quiet_fit(two_level_spec())), collapse = "\n")
  expect_match(report, "## Coefficient Key", fixed = TRUE)
  expect_match(report, "gamma_11 | ses_CWC:sectorPublic", fixed = TRUE)
})

test_that("validation benchmarks reproduce the stored AIC values", {
  benchmarks <- mlm_validation_benchmarks(run = TRUE)
  expect_true(all(benchmarks$`Formula Match`))
  expect_true(all(benchmarks$`Validation Status` == "Validated"))
})
