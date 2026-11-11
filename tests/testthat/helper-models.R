hsb <- example_hsb(n_schools = 16, min_students = 12, max_students = 18, seed = 101)

school_random <- function(slopes = character(), correlation = TRUE, intercept = TRUE) {
  list(schoolid = list(intercept = intercept, slopes = slopes, correlation = correlation))
}

two_level_spec <- function(data = hsb, ...) {
  mlm_spec(
    outcome = "mathscore",
    fixed = list(
      ses = list(center = "CWC"),
      meanses = list(center = "GMC"),
      sector = list(center = "none")
    ),
    grouping = list(schoolid = "schoolid"),
    random = school_random(slopes = "ses"),
    interactions = list(c("ses", "sector")),
    data = data,
    ...
  )
}

quiet_fit <- function(...) suppressWarnings(mlm_fit(...))

formula_text <- function(spec) {
  paste(deparse(mlm_formula(spec), width.cutoff = 500), collapse = " ")
}

# Runs the exported reproducible script (up to the optional imputation
# scaffold, which needs packages mlmr does not depend on) against `data`.
run_repro_code <- function(result, data) {
  code <- mlmr:::generate_repro_code(result)
  code <- code[seq_len(grep("Multiple Imputation Scaffold", code, fixed = TRUE)[[1]] - 2)]
  env <- new.env(parent = globalenv())
  env$dat <- data
  utils::capture.output(suppressMessages(suppressWarnings(eval(parse(text = code), env))))
  env
}
