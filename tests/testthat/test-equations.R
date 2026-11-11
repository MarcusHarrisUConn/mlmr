test_that("two-level equations place predictors at the level they vary", {
  result <- quiet_fit(two_level_spec())
  eq <- mlm_latex_equations(result)
  expect_identical(eq$notation, "two-level hierarchical")
  expect_identical(eq$level, c("Level 1", "Level 2 (schoolid)"))
  expect_identical(
    eq$equations[[1]],
    "\\mathrm{mathscore}_{ij} = \\beta_{0j} + \\beta_{1j}\\mathrm{ses\\_CWC}_{ij} + r_{ij}"
  )
  level2 <- strsplit(eq$equations[[2]], "\\\\ ", fixed = TRUE)[[1]]
  expect_identical(
    level2,
    c(
      "\\beta_{0j} = \\gamma_{00} + \\gamma_{01}\\mathrm{meanses\\_GMC}_{j} + \\gamma_{02}\\mathrm{sectorPublic}_{j} + u_{0j}",
      "\\beta_{1j} = \\gamma_{10} + \\gamma_{11}\\mathrm{sectorPublic}_{j} + u_{1j}"
    )
  )
  expect_match(eq$combined, "\\gamma_{11}\\mathrm{sectorPublic}_{j}\\mathrm{ses\\_CWC}_{ij}", fixed = TRUE)
  expect_match(eq$combined, "u_{1j}\\mathrm{ses\\_CWC}_{ij} + r_{ij}", fixed = TRUE)
  expect_match(eq$tau, "\\tau_{00} & \\tau_{10}\\\\ \\tau_{10} & \\tau_{11}", fixed = TRUE)
})

test_that("every fixed effect appears exactly once in the coefficient key", {
  result <- quiet_fit(two_level_spec())
  key <- mlm_latex_equations(result)$key
  fixed <- lme4::fixef(result$fit)
  expect_setequal(key$Coefficient, names(fixed))
  expect_false(anyDuplicated(key$Symbol) > 0)
  expect_equal(key$Estimate, unname(fixed[key$Coefficient]))
  expect_identical(key$Symbol[key$Coefficient == "ses_CWC:sectorPublic"], "gamma_11")
})

test_that("equation and Tau indices agree when a factor has several categories", {
  dat <- hsb
  dat$track <- factor(rep(c("a", "b", "c"), length.out = nrow(dat)))
  spec <- mlm_spec(
    "mathscore",
    fixed = list(track = list(), ses = list(center = "CWC")),
    grouping = list(schoolid = "schoolid"),
    random = school_random(slopes = "ses"),
    data = dat
  )
  result <- quiet_fit(spec)
  eq <- mlm_latex_equations(result)
  # Two dummy codes for track, then ses: the random slope belongs to beta_3j.
  expect_match(eq$equations[[1]], "\\beta_{1j}\\mathrm{trackb}_{ij} + \\beta_{2j}\\mathrm{trackc}_{ij} + \\beta_{3j}\\mathrm{ses\\_CWC}_{ij}", fixed = TRUE)
  expect_match(eq$equations[[2]], "\\beta_{3j} = \\gamma_{30} + u_{3j}", fixed = TRUE)
  expect_match(eq$tau, "u_{3j}", fixed = TRUE)
  expect_match(eq$tau, "\\tau_{33}", fixed = TRUE)
  labels <- mlmr:::tau_label_table(result)
  expect_identical(labels$`Matrix Index`, c("u_0j", "u_3j"))
})

test_that("independent random effects give a diagonal Tau matrix", {
  spec <- two_level_spec()
  spec$random <- school_random(slopes = "ses", correlation = FALSE)
  eq <- mlm_latex_equations(quiet_fit(spec))
  expect_match(eq$tau, "\\tau_{00} & 0\\\\ 0 & \\tau_{11}", fixed = TRUE)
})

test_that("three-level nested models use pi, beta, and gamma coefficients", {
  spec <- mlm_spec(
    "mathscore",
    fixed = list(ses = list(center = "CWC"), climate = list(center = "GMC")),
    grouping = list(schoolid = "schoolid", districtid = "districtid"),
    random = list(
      schoolid = list(intercept = TRUE, slopes = "ses", correlation = TRUE),
      districtid = list(intercept = TRUE)
    ),
    interactions = list(c("ses", "climate")),
    data = hsb
  )
  eq <- mlm_latex_equations(quiet_fit(spec))
  expect_identical(eq$notation, "three-level hierarchical")
  expect_identical(eq$level, c("Level 1", "Level 2 (schoolid)", "Level 3 (districtid)"))
  expect_identical(
    eq$equations[[1]],
    "\\mathrm{mathscore}_{ijk} = \\pi_{0jk} + \\pi_{1jk}\\mathrm{ses\\_CWC}_{ijk} + e_{ijk}"
  )
  expect_match(eq$equations[[2]], "\\pi_{0jk} = \\beta_{00k} + \\beta_{01k}\\mathrm{climate\\_GMC}_{jk} + r_{0jk}", fixed = TRUE)
  expect_match(eq$equations[[2]], "\\pi_{1jk} = \\beta_{10k} + \\beta_{11k}\\mathrm{climate\\_GMC}_{jk} + r_{1jk}", fixed = TRUE)
  expect_match(eq$equations[[3]], "\\beta_{00k} = \\gamma_{000} + u_{00k}", fixed = TRUE)
  expect_length(eq$tau, 2)
  expect_match(eq$tau[[1]], "\\tau_{\\pi 10}", fixed = TRUE)
  expect_match(eq$tau[[2]], "u_{00k}", fixed = TRUE)
})

test_that("crossed designs fall back to a single mixed-model equation", {
  spec <- mlm_spec(
    "mathscore",
    fixed = list(ses = list(center = "CWC")),
    grouping = list(schoolid = "schoolid", teacherid = "teacherid"),
    structure = "crossed",
    random = list(schoolid = list(intercept = TRUE), teacherid = list(intercept = TRUE)),
    data = hsb
  )
  eq <- mlm_latex_equations(quiet_fit(spec))
  expect_identical(eq$notation, "mixed-model")
  expect_identical(eq$level, "Mixed-model form")
  expect_match(eq$combined, "\\beta_{1}\\mathrm{ses\\_CWC}_{i}", fixed = TRUE)
  expect_true(endsWith(eq$combined, "+ e_{i}"))
  expect_length(eq$tau, 2)
})

test_that("generalized models are written on the link scale without a residual", {
  spec <- mlm_spec(
    "passmath", "binomial", "logit",
    fixed = list(ses = list(center = "CWC"), sector = list()),
    grouping = list(schoolid = "schoolid"),
    random = school_random(),
    data = hsb
  )
  result <- quiet_fit(spec)
  eq <- mlm_latex_equations(result)
  expect_match(eq$equations[[1]], "\\eta_{ij} = \\mathrm{logit}\\left[P(\\mathrm{passmath}_{ij} = 1)\\right]", fixed = TRUE)
  expect_false(grepl("r_{ij}", eq$combined, fixed = TRUE))
  expect_match(mlm_equations(result)[[1]], "logit(E[passmath])", fixed = TRUE)
})

test_that("equations render to self-contained HTML", {
  eq <- mlm_latex_equations(quiet_fit(two_level_spec()))
  html <- mlmr:::equation_html(c(eq$equations, eq$combined, eq$tau))
  expect_match(html, "&gamma;<sub>11</sub>", fixed = TRUE)
  expect_match(html, "eq-matrix", fixed = TRUE)
  expect_false(grepl("\\", html, fixed = TRUE))
})

test_that("plain-text equations keep the formula on one line", {
  lines <- mlm_equations(quiet_fit(two_level_spec()))
  expect_length(grep("^Fitted formula:", lines), 1)
  expect_match(lines[[1]], "b4*ses_CWC:sectorPublic", fixed = TRUE)
})
