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

test_that("reproducible code runs for generalized models and suppresses classical Gaussian ICCs", {
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
  expect_identical(nrow(env$icc_table), 0L)
  expect_s3_class(env$dummy_table, "data.frame")
  expect_identical(nrow(env$dummy_table), 0L)
})

test_that("reproducible code handles quotes in factor levels without syntax errors", {
  dat_q <- hsb
  levels(dat_q$sector) <- c("Catholic \"A\"", "Public \"B\"")
  spec_q <- two_level_spec(data = dat_q)
  res_q <- quiet_fit(spec_q)
  env_q <- run_repro_code(res_q, dat_q)
  expect_equal(lme4::fixef(env_q$fit), lme4::fixef(res_q$fit), tolerance = 1e-7)
})

test_that("reporting text and write-up agree on REML vs ML and follow APA p-value grammar", {
  res_reml <- quiet_fit(two_level_spec(), REML = TRUE, maxfun = 15000)
  expect_match(mlm_methods_text(res_reml), "using restricted maximum likelihood in lme4", fixed = TRUE)
  expect_match(mlmr:::model_writeup(res_reml), "using restricted maximum likelihood", fixed = TRUE)
  expect_false(grepl("p = <", mlmr:::model_writeup(res_reml), fixed = TRUE))

  result <- quiet_fit(two_level_spec(), REML = FALSE, maxfun = 12345)
  expect_identical(result$settings$REML, FALSE)
  text <- mlm_methods_text(result)
  expect_match(text, "using maximum likelihood in lme4", fixed = TRUE)
  expect_match(mlmr:::model_writeup(result), "using maximum likelihood", fixed = TRUE)
  expect_match(text, "maximum of 12345 function evaluations", fixed = TRUE)
  # A stale argument cannot contradict the fit.
  expect_match(mlm_methods_text(result, REML = TRUE), "using maximum likelihood in lme4", fixed = TRUE)
  check <- mlm_reproducibility_check(result)
  expect_match(check$Evidence[check$Item == "Estimator settings captured"], "REML: FALSE", fixed = TRUE)
  bundle <- mlm_publication_bundle(result)
  expect_true(any(grepl("REML = FALSE", bundle$reproducible_code, fixed = TRUE)))
  expect_named(bundle, c("methods_text", "reproducibility_check", "software_statement", "formula", "apa_tables", "equations", "reproducible_code"))
})

test_that("Satterthwaite and Kenward-Roger denominator degrees of freedom work when available", {
  skip_if_not_installed("lmerTest")
  res_satt <- quiet_fit(two_level_spec(), df_method = "satterthwaite")
  expect_identical(res_satt$settings$df_method, "satterthwaite")
  tab_satt <- mlm_apa_tables(res_satt)$fixed_effects
  expect_true(all(c("Predictor", "b", "SE", "df", "t", "p", "95% CI") %in% names(tab_satt)))
  chk <- mlm_reproducibility_check(res_satt)
  expect_identical(chk$Status[chk$Item == "Inference labels reviewed"], "OK")
  env_satt <- run_repro_code(res_satt, hsb)
  expect_true("df" %in% names(env_satt$apa_table))

  if (requireNamespace("pbkrtest", quietly = TRUE)) {
    res_kr <- quiet_fit(two_level_spec(), df_method = "kenward-roger")
    expect_identical(res_kr$settings$df_method, "kenward-roger")
    expect_true("df" %in% names(mlm_apa_tables(res_kr)$fixed_effects))
  }
})

test_that("mlm_r2 and mlm_reliability compute valid effect-size and reliability summaries", {
  res_gauss <- quiet_fit(two_level_spec())
  r2_g <- mlm_r2(res_gauss)
  expect_true(r2_g$R2_marginal >= 0 && r2_g$R2_marginal <= 1)
  expect_true(r2_g$R2_conditional >= r2_g$R2_marginal && r2_g$R2_conditional <= 1)

  rel_g <- mlm_reliability(res_gauss)
  expect_identical(unique(rel_g$Group), "schoolid")
  expect_identical(rel_g$Term, c("(Intercept)", "ses_CWC"))
  expect_true(all(rel_g$Mean_Reliability > 0 & rel_g$Mean_Reliability < 1))

  logit <- quiet_fit(mlm_spec(
    "passmath", "binomial", "logit",
    fixed = list(ses = list(center = "GMC")),
    grouping = list(schoolid = "schoolid"), random = school_random(), data = hsb
  ))
  r2_b <- mlm_r2(logit)
  expect_true(is.finite(r2_b$R2_conditional))
  expect_identical(nrow(mlm_reliability(logit)), 0L)
})

test_that("fixed-effects tables and GLMM bundles report exponentiated coefficients", {
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
  expect_match(mlm_apa_tables(logit, "latex", exponentiate = TRUE), "\\usepackage{amsmath}", fixed = TRUE)
  expect_match(mlm_apa_tables(logit, "html", exponentiate = TRUE), "exponentiated coefficient", fixed = TRUE)
  expect_true("OR" %in% names(mlm_publication_bundle(logit)$apa_tables$fixed_effects))

  counts <- quiet_fit(mlm_spec(
    "absences", "poisson", "log",
    fixed = list(ses = list(center = "GMC")),
    grouping = list(schoolid = "schoolid"), random = school_random(), data = hsb
  ))
  expect_true("IRR" %in% names(mlm_apa_tables(counts, exponentiate = TRUE)$fixed_effects))

  gaussian <- quiet_fit(two_level_spec())
  expect_named(mlm_apa_tables(gaussian, exponentiate = TRUE)$fixed_effects, names(plain))
})

test_that("ICCs are reported for Gaussian models only by default", {
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

test_that("manuscript report escapes formula pipes, wraps aligned equations, and writes .zip bundles", {
  res <- quiet_fit(two_level_spec())
  report <- paste(mlmr:::manuscript_report_markdown(res), collapse = "\n")
  expect_match(report, "## Coefficient Key", fixed = TRUE)
  expect_match(report, "## Fixed Effects With Cluster-Robust Standard Errors (CR1)", fixed = TRUE)
  expect_match(report, "## Random-Coefficient Reliability", fixed = TRUE)
  expect_match(report, "gamma_11 | ses_CWC:sectorPublic", fixed = TRUE)
  expect_match(report, "(1 + ses_CWC \\| schoolid)", fixed = TRUE)
  expect_match(report, "\\begin{aligned}", fixed = TRUE)

  zip_file <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_file), add = TRUE)
  mlm_write_publication_bundle(res, zip_file)
  expect_true(file.exists(zip_file))
  entries <- basename(utils::unzip(zip_file, list = TRUE)$Name)
  expect_true(all(c(
    "README_bundle.md",
    "mlmr_model_report.qmd",
    "mlmr_reproducible_analysis.R",
    "mlmr_tables_and_equations.tex",
    "mlmr_apa_tables.html",
    "reproducibility_checklist.csv",
    "software_versions.csv",
    "level1_residuals.csv",
    "cluster_eb_residuals.csv"
  ) %in% entries))
})

test_that("all 8 validation benchmarks reproduce stored estimates, SEs, variances, ICCs, and code round-trips", {
  benchmarks <- mlm_validation_benchmarks(run = TRUE)
  expect_identical(nrow(benchmarks), 8L)
  expect_true(all(benchmarks$`Formula Match`))
  expect_true(all(benchmarks$`Repro Code Match` == "Yes"))
  expect_true(all(benchmarks$`Validation Status` == "Validated"))
})
