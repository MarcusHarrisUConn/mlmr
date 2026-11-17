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
  dat$schoolnum <- stats::ave(as.integer(dat$schoolid), dat$districtid, FUN = function(x) as.integer(factor(x)))
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

test_that("readiness checks flag omitted interaction main effects, constant predictors, high-cardinality factors, and continuous group IDs", {
  # Interaction without main effect in fixed
  inter_spec <- mlm_spec(
    "mathscore",
    fixed = list(ses = list(center = "CWC")),
    grouping = list(schoolid = "schoolid"),
    random = school_random(),
    interactions = list(c("ses", "sector")),
    data = hsb
  )
  tab_inter <- readiness(inter_spec)
  expect_true("Interaction main effects" %in% tab_inter$Check)
  expect_identical(tab_inter$Status[tab_inter$Check == "Interaction main effects"], "Warn")

  # Constant predictor -> Stop
  dat_const <- hsb
  dat_const$const_x <- 5
  const_spec <- mlm_spec(
    "mathscore",
    fixed = list(const_x = list()),
    grouping = list(schoolid = "schoolid"),
    random = school_random(),
    data = dat_const
  )
  tab_const <- readiness(const_spec)
  expect_true(mlmr:::model_readiness_has_stops(tab_const))
  expect_true(any(grepl("Predictor variation: const_x", tab_const$Check, fixed = TRUE)))

  # High-cardinality categorical fixed predictor -> Warn
  high_card_spec <- mlm_spec(
    "mathscore",
    fixed = list(studentid = list()),
    grouping = list(schoolid = "schoolid"),
    random = school_random(),
    data = hsb
  )
  tab_hc <- readiness(high_card_spec)
  expect_true(any(grepl("High-cardinality predictor: studentid", tab_hc$Check, fixed = TRUE)))

  # Continuous numeric grouping variable -> Warn
  cont_grp_spec <- mlm_spec(
    "mathscore",
    grouping = list(ses = "ses"),
    random = list(ses = list(intercept = TRUE)),
    data = hsb
  )
  tab_cg <- readiness(cont_grp_spec)
  expect_true(any(grepl("Grouping coding: ses", tab_cg$Check, fixed = TRUE)))
})

test_that("three-level slope variation checks only the grouping factor carrying the slope", {
  spec_3l <- mlm_spec(
    "mathscore",
    fixed = list(ses = list(center = "CWC"), meanses = list(center = "GMC"), districtses = list(center = "GMC")),
    grouping = list(schoolid = "schoolid", districtid = "districtid"),
    random = list(
      schoolid = list(intercept = TRUE, slopes = "ses", correlation = TRUE),
      districtid = list(intercept = TRUE, slopes = "meanses", correlation = TRUE)
    ),
    data = hsb
  )
  tab_3l <- readiness(spec_3l)
  expect_false(mlmr:::model_readiness_has_stops(tab_3l))

  lvl_tab <- mlmr:::predictor_level_table(hsb, spec_3l)
  expect_match(lvl_tab$`Within-Group Variation`[lvl_tab$Predictor == "meanses"], "Constant within schoolid; Varies within districtid", fixed = TRUE)
  expect_match(lvl_tab$`Within-Group Variation`[lvl_tab$Predictor == "districtses"], "Constant within schoolid; Constant within districtid", fixed = TRUE)
})
