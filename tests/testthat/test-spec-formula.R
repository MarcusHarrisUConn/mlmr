test_that("mlm_spec validates its arguments", {
  expect_error(mlm_spec(outcome = c("a", "b")), "single variable name")
  expect_error(mlm_spec("mathscore", distribution = "weibull"), "Unsupported distribution")
  expect_error(mlm_spec("mathscore", fixed = list(list(center = "GMC"))), "named list")
  expect_error(mlm_spec("mathscore", data = "not a data frame"), "data frame")
  expect_error(
    mlm_spec("mathscore", fixed = list(not_in_data = list()), data = hsb),
    "missing from `data`"
  )
  expect_s3_class(two_level_spec(), "mlm_spec")
})

test_that("formulas reflect centering, interactions, and random effects", {
  expect_identical(
    formula_text(two_level_spec()),
    "mathscore ~ ses_CWC + meanses_GMC + sector + ses_CWC:sector + (1 + ses_CWC | schoolid)"
  )
  uncorrelated <- two_level_spec()
  uncorrelated$random <- school_random(slopes = "ses", correlation = FALSE)
  expect_identical(
    formula_text(uncorrelated),
    "mathscore ~ ses_CWC + meanses_GMC + sector + ses_CWC:sector + (1 | schoolid) + (0 + ses_CWC | schoolid)"
  )
  null_spec <- mlm_spec("mathscore", grouping = list(schoolid = "schoolid"), random = school_random(), data = hsb)
  expect_identical(formula_text(null_spec), "mathscore ~ 1 + (1 | schoolid)")
})

test_that("an empty random block never produces an invalid term", {
  spec <- mlm_spec(
    "mathscore",
    grouping = list(schoolid = "schoolid"),
    random = school_random(intercept = FALSE),
    data = hsb
  )
  expect_identical(formula_text(spec), "mathscore ~ 1")
  expect_error(mlm_fit(spec), "No random effects are specified")
})

test_that("non-syntactic variable names are quoted and can be fit", {
  dat <- hsb
  names(dat)[names(dat) == "ses"] <- "family ses"
  names(dat)[names(dat) == "mathscore"] <- "math score"
  names(dat)[names(dat) == "schoolid"] <- "school id"
  spec <- mlm_spec(
    "math score",
    fixed = list(`family ses` = list(center = "CWC"), sector = list()),
    grouping = list(`school id` = "school id"),
    random = list(`school id` = list(intercept = TRUE, slopes = "family ses", correlation = TRUE)),
    interactions = list(c("family ses", "sector")),
    data = dat
  )
  expect_match(formula_text(spec), "`math score` ~ `family ses_CWC`", fixed = TRUE)
  expect_match(formula_text(spec), "(1 + `family ses_CWC` | `school id`)", fixed = TRUE)
  result <- quiet_fit(spec)
  expect_s4_class(result$fit, "lmerMod")
  expect_no_error(mlm_publication_bundle(result))
  expect_false(any(grepl("`", mlm_latex_equations(result)$equations, fixed = TRUE)))
})

test_that("mlm_fit explains specifications it cannot fit", {
  expect_error(mlm_fit(list(outcome = "mathscore")), "mlm_spec")
  expect_error(
    mlm_fit(mlm_spec("mathscore", grouping = list(schoolid = "schoolid"), random = school_random())),
    "has no data"
  )
  expect_error(
    mlm_fit(mlm_spec("mathscore", fixed = list(sector = list(center = "GMC")),
      grouping = list(schoolid = "schoolid"), random = school_random(), data = hsb)),
    "sector is not numeric"
  )
  expect_error(
    mlm_fit(mlm_spec("mathscore", fixed = list(ses = list(center = "median")),
      grouping = list(schoolid = "schoolid"), random = school_random(), data = hsb)),
    "must be one of"
  )
  expect_error(
    mlm_fit(mlm_spec("mathscore", grouping = list(schoolid = "schoolid"),
      random = school_random(slopes = "ses"), data = hsb)),
    "must also be fixed predictors"
  )
  expect_error(
    mlm_fit(mlm_spec("mathscore", "binomial", "identity", grouping = list(schoolid = "schoolid"),
      random = school_random(), data = hsb)),
    "Unsupported distribution/link"
  )
})

test_that("print methods summarise specifications and results", {
  spec <- two_level_spec()
  expect_output(print(spec), "<mlm_spec>")
  expect_output(print(spec), "ses (CWC)", fixed = TRUE)
  result <- quiet_fit(spec)
  expect_s3_class(result, "mlm_result")
  expect_output(print(result), "Observations: ")
  expect_output(print(result), "ses_CWC:sectorPublic", fixed = TRUE)
})

test_that("example_hsb is reproducible and leaves the random stream alone", {
  expect_identical(example_hsb(n_schools = 4, seed = 1), example_hsb(n_schools = 4, seed = 1))
  set.seed(99)
  expected <- stats::runif(1)
  set.seed(99)
  invisible(example_hsb(n_schools = 4, seed = 1))
  expect_identical(stats::runif(1), expected)
  expect_error(example_hsb(seed = "a"), "single numeric value or NULL")
  expect_s3_class(example_hsb(n_schools = 4, seed = NULL), "data.frame")
})
