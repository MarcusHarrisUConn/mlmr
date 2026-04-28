`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  app_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
  setwd(app_dir)
}

shiny::runApp(".", launch.browser = TRUE)
