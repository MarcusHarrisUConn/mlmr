test_that("centering yields zero means in the analysis sample", {
  result <- quiet_fit(two_level_spec())
  frame <- stats::model.frame(result$fit)
  expect_lt(abs(mean(frame$meanses_GMC)), 1e-10)
  expect_lt(max(abs(tapply(frame$ses_CWC, frame$schoolid, mean))), 1e-10)
})

test_that("centering uses only rows that enter the model", {
  dat <- hsb
  set.seed(42)
  dat$mathscore[sample(nrow(dat), 40)] <- NA
  dat$ses[sample(nrow(dat), 20)] <- NA
  result <- quiet_fit(two_level_spec(data = dat))
  frame <- stats::model.frame(result$fit)
  expect_lt(nrow(frame), nrow(dat))
  expect_lt(abs(mean(frame$meanses_GMC)), 1e-10)
  expect_lt(max(abs(tapply(frame$ses_CWC, frame$schoolid, mean))), 1e-10)
  # Rows outside the analysis sample are kept, not dropped from the returned data.
  expect_identical(nrow(result$data), nrow(dat))
})

test_that("cluster-mean centering can target a higher-level grouping factor", {
  spec <- mlm_spec(
    "mathscore",
    fixed = list(ses = list(center = "CWC", group = "districtid")),
    grouping = list(schoolid = "schoolid", districtid = "districtid"),
    random = list(schoolid = list(intercept = TRUE), districtid = list(intercept = TRUE)),
    data = hsb
  )
  result <- quiet_fit(spec)
  expect_lt(max(abs(tapply(result$data$ses_CWC, result$data$districtid, mean))), 1e-10)
  expect_gt(max(abs(tapply(result$data$ses_CWC, result$data$schoolid, mean))), 1e-6)
  expect_match(result$centering_code, "dat$districtid", fixed = TRUE)
})
