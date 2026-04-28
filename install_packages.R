packages <- c("shiny", "bslib", "lme4", "ggplot2")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

if (!length(missing)) {
  message("All required packages are already installed.")
} else {
  install.packages(missing, repos = "https://cloud.r-project.org")
}
