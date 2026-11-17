# Development launcher so `shiny::runApp(".")` works directly from the
# repository root without requiring duplicate source copies.
required_packages <- c("shiny", "bslib", "lme4", "ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Install with install.packages(c(",
    paste(sprintf("\"%s\"", missing_packages), collapse = ", "),
    ")), then relaunch the app.",
    call. = FALSE
  )
}

local_env <- new.env(parent = globalenv())
local_env$.data <- ggplot2::.data
for (path in sort(list.files("R", pattern = "\\.[Rr]$", full.names = TRUE))) {
  sys.source(path, envir = local_env)
}
local_env$mlmr_shiny_app()
