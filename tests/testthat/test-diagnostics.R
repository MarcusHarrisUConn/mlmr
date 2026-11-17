test_that("diagnostic_frame aligns complete-case rows when data contain NAs", {
  dat_na <- hsb
  dat_na$mathscore[1:12] <- NA
  dat_na$ses[15:20] <- NA
  res_na <- quiet_fit(two_level_spec(data = dat_na))
  df <- mlmr:::diagnostic_frame(res_na)
  expect_identical(nrow(df), stats::nobs(res_na$fit))
  expect_false(anyNA(df$fitted))
  expect_false(anyNA(df$residual))
})

test_that("diagnostic_frame maps binary factor outcomes to 0/1 probabilities", {
  spec_bin <- mlm_spec(
    "passmath", "binomial", "logit",
    fixed = list(ses = list(center = "CWC")),
    grouping = list(schoolid = "schoolid"),
    random = school_random(),
    data = hsb
  )
  res_bin <- quiet_fit(spec_bin)
  df_bin <- mlmr:::diagnostic_frame(res_bin)
  expect_true(all(df_bin$observed %in% c(0, 1)))
  expect_true(all(df_bin$fitted >= 0 & df_bin$fitted <= 1))
})

test_that("compare_models_lrt computes likelihood-ratio tests and catches incompatible models", {
  m1 <- quiet_fit(mlm_spec("mathscore", grouping = list(schoolid = "schoolid"), random = school_random(), data = hsb))
  m2 <- quiet_fit(mlm_spec("mathscore", fixed = list(ses = list(center = "CWC")), grouping = list(schoolid = "schoolid"), random = school_random(), data = hsb))
  cmp_ok <- mlmr:::compare_models_lrt(list("Model 1" = m1, "Model 2" = m2))
  expect_true(cmp_ok$ok)
  expect_true("Chisq" %in% names(cmp_ok$table))
  expect_identical(cmp_ok$table$Model, c("Model 1", "Model 2"))

  # Different outcome -> gracefully refused
  m_bin <- quiet_fit(mlm_spec("passmath", "binomial", "logit", grouping = list(schoolid = "schoolid"), random = school_random(), data = hsb))
  cmp_bad_out <- mlmr:::compare_models_lrt(list("Gaussian" = m1, "Logit" = m_bin))
  expect_false(cmp_bad_out$ok)

  # Different complete-case sample size -> gracefully refused
  dat_sub <- hsb
  dat_sub$ses[1:10] <- NA
  m2_na <- quiet_fit(mlm_spec("mathscore", fixed = list(ses = list(center = "CWC")), grouping = list(schoolid = "schoolid"), random = school_random(), data = dat_sub))
  cmp_bad_n <- mlmr:::compare_models_lrt(list("Full N" = m1, "Smaller N" = m2_na))
  expect_false(cmp_bad_n$ok)
})

test_that("Robust SEs, multiparameter Wald tests, variance LRTs, homogeneity H, residual files, and marginal ORs work accurately", {
  res <- quiet_fit(two_level_spec())
  rob_cr1 <- mlm_robust_se(res, type = "CR1")
  rob_cr0 <- mlm_robust_se(res, type = "CR0")
  expect_identical(rob_cr1$Predictor, names(lme4::fixef(res$fit)))
  expect_equal(rob_cr1$SE_Model, unname(round(sqrt(diag(as.matrix(stats::vcov(res$fit)))), 4)), tolerance = 1e-4)
  J <- rob_cr1$Clusters[[1]]
  expect_equal(rob_cr1$SE_Robust, rob_cr0$SE_Robust * sqrt(J / (J - 1)), tolerance = 1e-3)
  expect_identical(rob_cr1$df, rep(J - 1, nrow(rob_cr1)))

  wt_mod <- mlm_wald_test(res, terms = c("sectorPublic", "ses_CWC:sectorPublic"), vcov_type = "model")
  wt_cr1 <- mlm_wald_test(res, terms = c("sectorPublic", "ses_CWC:sectorPublic"), vcov_type = "CR1")
  expect_identical(wt_mod$df, 2L)
  expect_identical(wt_cr1$df, 2L)
  expect_true(wt_mod$Chisq > 0 && wt_cr1$Chisq > 0)

  rf <- mlm_residual_files(res)
  expect_identical(nrow(rf$level1), stats::nobs(res$fit))
  expect_true(all(c("EB_Estimate", "Posterior_SE") %in% names(rf$cluster)))

  vt <- mlm_variance_tests(res)
  expect_identical(vt$Term, c("(Intercept)", "ses_CWC"))
  expect_true(all(vt$Chisq >= 0))
  expect_true(all(nzchar(vt$p)))

  ht <- mlm_homogeneity_test(res)
  expect_identical(nrow(ht), 1L)
  expect_identical(ht$Cluster, "schoolid")
  expect_true(is.finite(ht$H_Statistic) && ht$H_Statistic >= 0)
  expect_identical(mlm_population_average(res)$Predictor, character())

  # Binary logit HGLM: robust SEs + population-average shrinkage
  spec_bin <- mlm_spec(
    "passmath", "binomial", "logit",
    fixed = list(ses = list(center = "CWC"), sector = list()),
    grouping = list(schoolid = "schoolid"),
    random = school_random(),
    data = hsb
  )
  res_bin <- quiet_fit(spec_bin)
  rob_bin <- mlm_robust_se(res_bin)
  expect_true("OR" %in% names(rob_bin))
  pa <- mlm_population_average(res_bin)
  expect_identical(nrow(pa), length(lme4::fixef(res_bin$fit)))
  expect_true(all(abs(pa$b_PopAverage) <= abs(pa$b_UnitSpecific) + 1e-3))
  expect_identical(nrow(mlm_homogeneity_test(res_bin)), 0L)
})
