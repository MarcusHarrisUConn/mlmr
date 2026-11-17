test_that("the bundled app delegates directly to package code without duplicate copies", {
  app_dir <- system.file("app", package = "mlmr")
  skip_if(!nzchar(app_dir), "Bundled app is not installed.")
  expect_true(file.exists(file.path(app_dir, "app.R")))
  expect_true(file.exists(file.path(app_dir, "www", "style.css")))
  expect_false(dir.exists(file.path(app_dir, "R")))
  expect_no_error(parse(file.path(app_dir, "app.R")))
  app_obj <- mlmr:::mlmr_shiny_app()
  expect_s3_class(app_obj, "shiny.appobj")
})

test_that("stylesheet defines dark-mode equation contrast and distinct primary buttons", {
  css_path <- system.file("app", "www", "style.css", package = "mlmr")
  skip_if(!nzchar(css_path), "Stylesheet not installed.")
  css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
  expect_match(css, "\\[data-bs-theme=\"dark\"\\] \\.equation-rendered", perl = TRUE)
  expect_match(css, "\\.btn-primary\\s*\\{[^}]*var\\(--mlmr-navy\\)", perl = TRUE)
})

test_that("the bundled app fits a non-singular example model and renders every output", {
  skip_on_cran()
  app_obj <- mlmr:::mlmr_shiny_app()
  server_lines <- deparse(body(mlmr:::app_server), width.cutoff = 500)
  outputs <- unique(sub(
    ".*output\\$([A-Za-z0-9_]+)\\s*<-.*", "\\1",
    grep("output\\$[A-Za-z0-9_]+\\s*<-\\s*(shiny::)?(render|download)", server_lines, value = TRUE)
  ))
  expect_gt(length(outputs), 40)

  failures <- character()
  result_obj <- NULL
  suppressWarnings(suppressMessages(shiny::testServer(app_obj, {
    session$setInputs(
      use_example_data = TRUE,
      example_dataset = "hsb",
      advanced_mode = FALSE,
      reml = TRUE,
      df_method = "none",
      optimizer = "bobyqa",
      maxfun = 20000,
      strings_as_factors = TRUE
    )
    session$setInputs(fit_example = 1)
    session$flushReact()
    result_obj <<- active_result()
    for (name in outputs) {
      # Skip file downloadHandlers here; download archive tested separately below.
      if (startsWith(name, "download_")) next
      problem <- tryCatch(
        {
          output[[name]]
          NULL
        },
        shiny.silent.error = function(e) NULL,
        error = function(e) conditionMessage(e)
      )
      if (!is.null(problem)) failures <<- c(failures, paste0(name, ": ", problem))
    }
  })))
  expect_s3_class(result_obj, "mlm_result")
  expect_false(lme4::isSingular(result_obj$fit, tol = 1e-4))
  expect_identical(failures, character())
})

test_that("uploading custom CSV and SPSS files resets stale HSB defaults cleanly", {
  skip_on_cran()
  app_obj <- mlmr:::mlmr_shiny_app()
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)
  utils::write.csv(
    data.frame(
      score = stats::rnorm(48, 50, 8),
      effort = stats::rnorm(48, 0, 1),
      cluster = rep(sprintf("C%02d", 1:12), each = 4)
    ),
    tmp_csv,
    row.names = FALSE
  )

  shiny::testServer(app_obj, {
    # Populate browser-like HSB initial inputs including default interactions.
    session$setInputs(
      use_example_data = TRUE,
      example_dataset = "hsb",
      strings_as_factors = TRUE,
      advanced_mode = FALSE,
      outcome = "mathscore",
      grouping_vars = "schoolid",
      level1_terms = c("ses", "homework"),
      level2_terms = "sector",
      level3_terms = character(),
      interactions = "ses:sector, homework:climate"
    )
    session$flushReact()

    # Upload custom CSV without manually clearing interactions first.
    session$setInputs(
      use_example_data = FALSE,
      data_file = list(name = "custom.csv", datapath = tmp_csv),
      outcome = "score",
      grouping_vars = "cluster",
      level1_terms = "effort",
      level2_terms = character(),
      level3_terms = character()
    )
    session$flushReact()

    expect_no_error(spec_reactive())
    expect_false(mlmr:::model_readiness_has_stops(readiness_reactive()))
  })

  if (requireNamespace("haven", quietly = TRUE)) {
    sav_path <- tempfile(fileext = ".sav")
    on.exit(unlink(sav_path), add = TRUE)
    df_sav <- data.frame(
      y = stats::rnorm(40),
      grp = haven::labelled(rep(1:10, each = 4), labels = stats::setNames(1:10, paste0("G", 1:10)), label = "Cluster ID"),
      arm = haven::labelled(rep(c(0, 1), 20), labels = c(Control = 0, Active = 1), label = "Study Arm")
    )
    haven::write_sav(df_sav, sav_path)
    cleaned <- mlmr:::read_uploaded_datafile(sav_path, "study.sav", strings_as_factors = TRUE)
    expect_true(is.factor(cleaned$arm))
    expect_identical(levels(cleaned$arm), c("Control", "Active"))
    spec_sav <- mlm_spec("y", fixed = list(arm = list()), grouping = list(grp = "grp"), random = list(grp = list(intercept = TRUE)), data = cleaned)
    roles_tab <- mlmr:::variable_role_table(cleaned, spec_sav)
    expect_true("Label" %in% names(roles_tab))
    expect_identical(roles_tab$Label[roles_tab$Variable == "arm"], "Study Arm")
  }
})

test_that("per-group random slopes persist across grouping factors and LRT comparison works", {
  skip_on_cran()
  app_obj <- mlmr:::mlmr_shiny_app()
  shiny::testServer(app_obj, {
    session$setInputs(
      use_example_data = TRUE,
      example_dataset = "hsb",
      strings_as_factors = TRUE,
      advanced_mode = FALSE,
      reml = TRUE,
      df_method = "none",
      optimizer = "bobyqa",
      maxfun = 20000,
      outcome = "mathscore",
      structure_type = "nested",
      grouping_vars = c("schoolid", "districtid"),
      random_intercept_groups = c("schoolid", "districtid"),
      level1_terms = c("ses", "homework"),
      level2_terms = "meanses",
      level3_terms = character(),
      interactions = "",
      random_group_focus = "schoolid",
      random_slopes = "ses",
      random_correlation = TRUE
    )
    session$flushReact()

    # Switch focus to districtid and assign homework slope without losing schoolid's ses slope.
    session$setInputs(random_group_focus = "districtid", random_slopes = "homework", random_correlation = FALSE)
    session$flushReact()
    f_txt <- paste(deparse(mlm_formula(spec_reactive()), width.cutoff = 500), collapse = " ")
    expect_match(f_txt, "(1 + ses_CWC | schoolid)", fixed = TRUE)
    expect_match(f_txt, "(0 + homework_CWC | districtid)", fixed = TRUE)

    # Fit two nested two-level models and verify LRT comparison works cleanly.
    session$setInputs(
      grouping_vars = "schoolid",
      random_intercept_groups = "schoolid",
      random_group_focus = "schoolid",
      random_slopes = character(),
      level1_terms = "ses",
      level2_terms = character()
    )
    session$flushReact()
    session$setInputs(fit_model = 1)
    session$flushReact()

    session$setInputs(level2_terms = "meanses")
    session$flushReact()
    session$setInputs(fit_model = 2)
    session$flushReact()

    cmp <- lrt_comparison()
    expect_true(cmp$ok)
    expect_true("Chisq" %in% names(cmp$table))
    expect_identical(nrow(cmp$table), 2L)

    session$setInputs(clear_model_history = 1)
    session$flushReact()
    expect_length(model_history(), 0L)
  })
})

test_that("design_mode, inline random-slope toggles, cross-level moderators, and direct non-Gaussian distributions work", {
  skip_on_cran()
  app_obj <- mlmr:::mlmr_shiny_app()
  suppressWarnings(suppressMessages(shiny::testServer(app_obj, {
    session$setInputs(
      use_example_data = TRUE,
      example_dataset = "hsb",
      strings_as_factors = TRUE,
      design_mode = "three_level",
      cluster_l2 = "schoolid",
      cluster_l3 = "districtid",
      outcome_data = "mathscore",
      outcome = "mathscore",
      distribution = "gaussian",
      link = "identity",
      level1_terms = c("ses", "homework"),
      level2_terms = "sector",
      level3_terms = "districtses",
      slope_l1_ses = TRUE,
      slope_l1_homework = FALSE,
      mod_l1_ses = "sector",
      mod_l1_homework = character(),
      advanced_mode = FALSE
    )
    session$flushReact()
    expect_identical(active_groups(), c("schoolid", "districtid"))
    f3 <- paste(deparse(mlm_formula(spec_reactive()), width.cutoff = 500), collapse = " ")
    expect_match(f3, "districtses_GMC", fixed = TRUE)
    expect_match(f3, "ses_CWC:sector", fixed = TRUE)
    expect_match(f3, "(1 + ses_CWC | schoolid)", fixed = TRUE)
    expect_match(f3, "(1 | districtid)", fixed = TRUE)

    # Switch directly to Binomial Logit on passmath WITHOUT needing advanced_mode = TRUE
    session$setInputs(
      advanced_mode = FALSE,
      design_mode = "two_level",
      cluster_l2 = "schoolid",
      outcome = "passmath",
      distribution = "binomial",
      link = "logit",
      level1_terms = "ses",
      level2_terms = "sector",
      level3_terms = character(),
      slope_l1_ses = FALSE,
      mod_l1_ses = character(),
      interactions = "",
      reml = FALSE,
      df_method = "none",
      optimizer = "bobyqa",
      maxfun = 20000
    )
    session$flushReact()
    expect_identical(spec_reactive()$distribution, "binomial")
    expect_identical(spec_reactive()$link, "logit")
    session$setInputs(fit_model = 1)
    session$flushReact()
    expect_s3_class(active_result(), "mlm_result")
    expect_no_error(output$pop_average_card_ui)
    expect_no_error(output$wald_test_table)
  })))
})
