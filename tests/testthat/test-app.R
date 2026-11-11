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
