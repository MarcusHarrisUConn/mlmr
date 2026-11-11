test_that("the bundled app runs the same core code as the package", {
  app_core <- system.file("app", "R", "mlm_core.R", package = "mlmr")
  skip_if(!nzchar(app_core), "Bundled app is not installed.")
  env <- new.env(parent = globalenv())
  sys.source(app_core, envir = env)
  app_functions <- ls(env, all.names = TRUE)
  namespace <- asNamespace("mlmr")
  expect_true(all(app_functions %in% ls(namespace, all.names = TRUE)))
  drifted <- Filter(function(name) {
    !identical(deparse(body(get(name, env))), deparse(body(get(name, namespace)))) ||
      !identical(formals(get(name, env)), formals(get(name, namespace)))
  }, app_functions)
  expect_identical(drifted, character())
})

test_that("the bundled app ships its script and stylesheet", {
  app_dir <- system.file("app", package = "mlmr")
  skip_if(!nzchar(app_dir), "Bundled app is not installed.")
  expect_true(file.exists(file.path(app_dir, "app.R")))
  expect_true(file.exists(file.path(app_dir, "www", "style.css")))
  expect_no_error(parse(file.path(app_dir, "app.R")))
})

test_that("the bundled app fits the example and renders every output", {
  skip_on_cran()
  app_dir <- system.file("app", package = "mlmr")
  skip_if(!nzchar(app_dir), "Bundled app is not installed.")
  source_lines <- readLines(file.path(app_dir, "app.R"), warn = FALSE)
  outputs <- unique(sub(
    ".*output\\$([A-Za-z0-9_]+) <- .*", "\\1",
    grep("output\\$[A-Za-z0-9_]+ <- (render|download)", source_lines, value = TRUE)
  ))
  expect_gt(length(outputs), 50)
  failures <- character()
  result_class <- NULL
  suppressWarnings(suppressMessages(shiny::testServer(app_dir, {
    session$setInputs(
      data_source = "example", advanced_mode = FALSE, reml = TRUE,
      optimizer = "bobyqa", maxfun = 20000, strings_as_factors = TRUE
    )
    session$setInputs(fit_example = 1)
    session$flushReact()
    result_class <<- class(active_result())
    for (name in outputs) {
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
  expect_identical(result_class, "mlm_result")
  expect_identical(failures, character())
})
