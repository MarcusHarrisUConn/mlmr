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
