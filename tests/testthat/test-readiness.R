readiness <- function(spec, data = spec$data) mlmr:::model_readiness_table(data, spec)

test_that("a sound specification has no stop items", {
  tab <- readiness(two_level_spec())
  expect_false(mlmr:::model_readiness_has_stops(tab))
  expect_named(tab, c("Status", "Check", "Detail"))
})

test_that("specification problems surface as stop items", {
  spec <- two_level_spec()
  spec$fixed$sector$center <- "GMC"
  tab <- readiness(spec)
  expect_true(mlmr:::model_readiness_has_stops(tab))
  expect_true(any(grepl("sector is not numeric", tab$Detail, fixed = TRUE)))

  no_random <- mlm_spec("mathscore", grouping = list(schoolid = "schoolid"), data = hsb)
  expect_true(any(grepl("No random effects", readiness(no_random)$Detail, fixed = TRUE)))
})

test_that("reused lower-level IDs are flagged for nested designs", {
  dat <- hsb
  dat$schoolnum <- ave(as.integer(dat$schoolid), dat$districtid, FUN = function(x) as.integer(factor(x)))
  reused <- mlm_spec(
    "mathscore",
    grouping = list(schoolnum = "schoolnum", districtid = "districtid"),
    random = list(schoolnum = list(intercept = TRUE), districtid = list(intercept = TRUE)),
    data = dat
  )
  row <- readiness(reused)
  row <- row[row$Check == "Nesting of grouping IDs", ]
  expect_identical(row$Status, "Warn")
  expect_match(row$Detail, "not nested as coded", fixed = TRUE)

  unique_ids <- mlm_spec(
    "mathscore",
    grouping = list(schoolid = "schoolid", districtid = "districtid"),
    random = list(schoolid = list(intercept = TRUE), districtid = list(intercept = TRUE)),
    data = hsb
  )
  row <- readiness(unique_ids)
  row <- row[row$Check == "Nesting of grouping IDs", ]
  expect_identical(row$Status, "OK")
  expect_match(row$Detail, "schoolid within districtid", fixed = TRUE)
})

test_that("a hierarchy declared as crossed is questioned", {
  check <- mlmr:::nesting_check(hsb, c("schoolid", "districtid"), "crossed")
  expect_identical(check$status, "Warn")
  expect_null(mlmr:::nesting_check(hsb, c("schoolid", "teacherid"), "crossed"))
})
