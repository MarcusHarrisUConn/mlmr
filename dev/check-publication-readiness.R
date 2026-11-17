message("Checking mlmr publication readiness...")

stop_if_not <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

stop_if_not(
  file.exists("inst/app/app.R") && file.exists("inst/app/www/style.css"),
  "Missing bundled app entrypoint or stylesheet under inst/app/."
)

stop_if_not(
  !dir.exists("inst/app/R") && !file.exists("www/style.css") && !file.exists("R/mlm_core.R"),
  "Legacy duplicated app/backend files detected. All package and Shiny code must live in single-source R/ modules."
)

code_files <- c(list.files("R", pattern = "\\.[Rr]$", full.names = TRUE), "app.R", "inst/app/app.R")
external_runtime_pattern <- "cdn\\.jsdelivr|mathjax\\.rstudio\\.com|MathJax\\.js|https://.*\\.js"
external_runtime_hits <- unlist(lapply(code_files[file.exists(code_files)], function(path) {
  lines <- readLines(path, warn = FALSE)
  hits <- grep(external_runtime_pattern, lines, value = TRUE)
  if (length(hits)) paste(path, hits, sep = ": ")
}))

stop_if_not(!length(external_runtime_hits), paste(c(
  "External runtime JavaScript references found in app/package files:",
  external_runtime_hits
), collapse = "\n"))

public_files <- c(
  "README.md",
  "BETA_TESTING.md",
  "DEMO.md",
  "CONTRIBUTING.md",
  "paper/paper.md",
  "paper/ARTICLE_READINESS.md",
  "paper/SCREENSHOT_CHECKLIST.md",
  "vignettes/getting-started.Rmd",
  "vignettes/supported-models.Rmd",
  "vignettes/equations-and-reporting.Rmd",
  "vignettes/validation-and-benchmarks.Rmd"
)

local_path_pattern <- "C:\\\\Users|AppData|mah22013|/Users/marcusharris"
local_path_hits <- unlist(lapply(public_files[file.exists(public_files)], function(path) {
  lines <- readLines(path, warn = FALSE)
  hits <- grep(local_path_pattern, lines, value = TRUE)
  if (length(hits)) paste(path, hits, sep = ": ")
}))

stop_if_not(!length(local_path_hits), paste(c(
  "Local machine path text found in public documentation:",
  local_path_hits
), collapse = "\n"))

required_vignettes <- c(
  "getting-started",
  "supported-models",
  "equations-and-reporting",
  "validation-and-benchmarks"
)

vignette_paths <- file.path("vignettes", paste0(required_vignettes, ".Rmd"))
missing_vignettes <- vignette_paths[!file.exists(vignette_paths)]
stop_if_not(!length(missing_vignettes), paste(c(
  "Required vignettes are missing:",
  missing_vignettes
), collapse = "\n"))

tarballs <- list.files(pattern = "^mlmr_.*[.]tar[.]gz$")
if (length(tarballs)) {
  latest_tarball <- tarballs[[which.max(file.info(tarballs)$mtime)]]
  files <- utils::untar(latest_tarball, list = TRUE)
  excluded_pattern <- paste(
    "PUBLICATION_READINESS",
    "CRAN_SUBMISSION",
    "cran-comments",
    "RELEASE",
    "DOCKER",
    "Dockerfile",
    "docker-compose",
    "dockerignore",
    "\\.github",
    "dev/",
    "docs/",
    "pkgdown",
    "paper/",
    "CITATION\\.cff",
    "codemeta\\.json",
    "Rplots",
    "C:\\\\Users",
    "AppData",
    "mah22013",
    "/Users/marcusharris",
    sep = "|"
  )
  artifact_hits <- grep(excluded_pattern, files, value = TRUE)
  stop_if_not(!length(artifact_hits), paste(c(
    paste("Excluded or local artifacts found in", latest_tarball, ":"),
    artifact_hits
  ), collapse = "\n"))

  built_vignette_hits <- unlist(lapply(required_vignettes, function(vig) {
    expected <- paste0("mlmr/inst/doc/", vig, c(".html", ".R", ".Rmd"))
    setdiff(expected, files)
  }))
  stop_if_not(!length(built_vignette_hits), paste(c(
    paste("Built vignette files are missing from", latest_tarball, ":"),
    built_vignette_hits
  ), collapse = "\n"))
}

required_files <- c(
  "DESCRIPTION",
  "NAMESPACE",
  "inst/CITATION",
  "cran-comments.md",
  "CRAN_SUBMISSION.md",
  "CITATION.cff",
  "codemeta.json",
  "paper/paper.md",
  "paper/paper.bib",
  "paper/ARTICLE_READINESS.md",
  "paper/SCREENSHOT_CHECKLIST.md",
  "BETA_TESTING.md",
  "PUBLICATION_READINESS.md",
  ".github/workflows/R-CMD-check.yaml",
  ".github/workflows/pkgdown.yaml",
  ".github/workflows/docker-image.yaml"
)

missing_required <- required_files[!file.exists(required_files)]
stop_if_not(!length(missing_required), paste(c(
  "Required release files are missing:",
  missing_required
), collapse = "\n"))

message("Publication readiness checks passed.")
