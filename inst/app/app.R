# Installed Shiny app entrypoint for `mlmr`.
# All UI, server, and modeling code live in the `mlmr` package namespace.
if (requireNamespace("mlmr", quietly = TRUE)) {
  getFromNamespace("mlmr_shiny_app", "mlmr")()
} else if (dir.exists(file.path("..", "..", "R"))) {
  env <- new.env(parent = globalenv())
  env$.data <- ggplot2::.data
  for (path in sort(list.files(file.path("..", "..", "R"), pattern = "\\.[Rr]$", full.names = TRUE))) {
    sys.source(path, envir = env)
  }
  env$mlmr_shiny_app()
} else {
  stop("Install the `mlmr` package (`pak::pak(\"MarcusHarrisUConn/mlmr\")`) before launching the app.", call. = FALSE)
}
