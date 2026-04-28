run_mlmr <- function(launch.browser = TRUE, port = getOption("shiny.port", NULL)) {
  app_dir <- system.file("app", package = "mlmr")
  if (!nzchar(app_dir)) {
    app_dir <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
  }
  shiny::runApp(app_dir, launch.browser = launch.browser, port = port)
}
