message("Checking mlmr publication readiness...")

stop_if_not <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

same_file <- function(path_a, path_b) {
  stop_if_not(file.exists(path_a), paste("Missing file:", path_a))
  stop_if_not(file.exists(path_b), paste("Missing file:", path_b))
  identical(readLines(path_a, warn = FALSE), readLines(path_b, warn = FALSE))
}

stop_if_not(
  same_file("R/mlm_core.R", "inst/app/R/mlm_core.R"),
  "Backend copy drift detected: R/mlm_core.R and inst/app/R/mlm_core.R differ."
)

stop_if_not(
  same_file("app.R", "inst/app/app.R"),
  "App copy drift detected: app.R and inst/app/app.R differ."
)

stop_if_not(
  same_file("www/style.css", "inst/app/www/style.css"),
  "CSS copy drift detected: www/style.css and inst/app/www/style.css differ."
)

public_files <- c(
  "README.md",
  "BETA_TESTING.md",
  "DEMO.md",
  "vignettes/getting-started.Rmd",
  "vignettes/supported-models.Rmd",
  "vignettes/equations-and-reporting.Rmd"
)

local_path_pattern <- "C:\\\\Users|AppData|mah22013"
local_path_hits <- unlist(lapply(public_files[file.exists(public_files)], function(path) {
  lines <- readLines(path, warn = FALSE)
  hits <- grep(local_path_pattern, lines, value = TRUE)
  if (length(hits)) paste(path, hits, sep = ": ")
}))

stop_if_not(!length(local_path_hits), paste(c(
  "Local machine path text found in public documentation:",
  local_path_hits
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
    "Rplots",
    "C:\\\\Users",
    "AppData",
    "mah22013",
    sep = "|"
  )
  artifact_hits <- grep(excluded_pattern, files, value = TRUE)
  stop_if_not(!length(artifact_hits), paste(c(
    paste("Excluded or local artifacts found in", latest_tarball, ":"),
    artifact_hits
  ), collapse = "\n"))
}

required_files <- c(
  "DESCRIPTION",
  "NAMESPACE",
  "inst/CITATION",
  "cran-comments.md",
  "CRAN_SUBMISSION.md",
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
