# Mirror the package sources into the bundled Shiny app.
#
# The app in inst/app/ runs from its own copies of the core code, the app
# script, and the stylesheet so it also works as a standalone folder (for
# example in the Docker image). Run this from the package root after editing
# R/mlm_core.R, app.R, or www/style.css:
#
#   Rscript dev/sync-app.R

pairs <- c(
  "R/mlm_core.R" = "inst/app/R/mlm_core.R",
  "app.R" = "inst/app/app.R",
  "www/style.css" = "inst/app/www/style.css"
)

if (!all(file.exists(names(pairs)))) {
  stop("Run dev/sync-app.R from the package root.", call. = FALSE)
}

for (source_path in names(pairs)) {
  target_path <- pairs[[source_path]]
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  same <- file.exists(target_path) &&
    identical(readLines(source_path, warn = FALSE), readLines(target_path, warn = FALSE))
  if (same) {
    message("Up to date: ", target_path)
  } else {
    file.copy(source_path, target_path, overwrite = TRUE)
    message("Updated:    ", target_path)
  }
}
