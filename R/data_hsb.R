example_hsb <- function(n_schools = 24, min_students = 18, max_students = 34, seed = 20260423) {
  if (!is.numeric(n_schools) || length(n_schools) != 1 || is.na(n_schools) || n_schools < 2) {
    stop("`n_schools` must be a single number greater than or equal to 2.", call. = FALSE)
  }
  if (!is.numeric(min_students) || !is.numeric(max_students) ||
      length(min_students) != 1 || length(max_students) != 1 ||
      is.na(min_students) || is.na(max_students) ||
      min_students < 1 || max_students < min_students) {
    stop("`min_students` and `max_students` must define a valid positive range.", call. = FALSE)
  }
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || is.na(seed))) {
    stop("`seed` must be a single numeric value or NULL.", call. = FALSE)
  }
  n_schools <- as.integer(n_schools)
  min_students <- as.integer(min_students)
  max_students <- as.integer(max_students)
  if (!is.null(seed)) {
    # Generate reproducible data without disturbing the caller's random stream.
    had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit({
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = globalenv())
      } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      }
    }, add = TRUE)
    set.seed(seed)
  }
  n_districts <- max(4, floor(n_schools / 4))
  school_to_district <- sprintf("D%02d", rep(seq_len(n_districts), length.out = n_schools))
  school_sizes <- sample(min_students:max_students, n_schools, replace = TRUE)
  schoolid <- rep(sprintf("S%02d", seq_len(n_schools)), school_sizes)
  districtid <- school_to_district[as.integer(factor(schoolid, levels = sprintf("S%02d", seq_len(n_schools))))]
  n <- length(schoolid)
  n_teachers <- max(12, round(n_schools * 1.6))
  teacher_pool <- sprintf("T%02d", seq_len(n_teachers))
  school_teacher_map <- lapply(seq_len(n_schools), function(i) {
    sample(teacher_pool, sample(3:6, 1), replace = FALSE)
  })
  names(school_teacher_map) <- sprintf("S%02d", seq_len(n_schools))
  teacherid <- vapply(schoolid, function(s) sample(school_teacher_map[[s]], 1), character(1))
  school_sector <- sample(c("Public", "Catholic"), n_schools, replace = TRUE)
  school_meanses <- stats::rnorm(n_schools, 0, 0.45)
  school_climate <- stats::rnorm(n_schools, 0, 1)
  school_size <- round(stats::rnorm(n_schools, 850, 180))
  names(school_sector) <- names(school_meanses) <- names(school_climate) <- names(school_size) <- sprintf("S%02d", seq_len(n_schools))
  u0 <- stats::rnorm(n_schools, 0, 5.5)
  u1 <- stats::rnorm(n_schools, 0, 1.2)
  u2 <- stats::rnorm(n_schools, 0, 0.7)
  u3 <- stats::rnorm(n_schools, 0, 0.5)
  d0 <- stats::rnorm(n_districts, 0, 2.4)
  t0 <- stats::rnorm(n_teachers, 0, 1.6)
  names(u0) <- names(u1) <- names(u2) <- names(u3) <- sprintf("S%02d", seq_len(n_schools))
  names(d0) <- sprintf("D%02d", seq_len(n_districts))
  names(t0) <- teacher_pool
  ses <- stats::rnorm(n, school_meanses[schoolid], 0.65)
  priorachieve <- 50 + 7 * ses + stats::rnorm(n, 0, 8)
  homework <- pmax(0, round(stats::rnorm(n, 5 + 0.8 * ses, 1.6), 1))
  meanses <- as.numeric(school_meanses[schoolid])
  climate <- as.numeric(school_climate[schoolid])
  size <- as.numeric(school_size[schoolid])
  sector <- factor(school_sector[schoolid])
  female <- factor(stats::rbinom(n, 1, 0.52), labels = c("Male", "Female"))
  minority <- factor(stats::rbinom(n, 1, 0.32), labels = c("No", "Yes"))
  mathscore <- 52 +
    3.1 * ses +
    0.34 * priorachieve +
    1.05 * homework +
    2.8 * meanses +
    1.6 * climate -
    0.004 * size +
    1.9 * (sector == "Catholic") +
    1.2 * (female == "Female") -
    1.7 * (minority == "Yes") +
    1.3 * ses * (sector == "Catholic") +
    0.45 * homework * climate +
    d0[districtid] +
    t0[teacherid] +
    u0[schoolid] +
    u1[schoolid] * ses +
    u2[schoolid] * homework +
    u3[schoolid] * (priorachieve / 10) +
    stats::rnorm(n, 0, 6.2)
  pass_prob <- stats::plogis(-0.8 + 0.055 * (mathscore - 50) + 0.45 * ses + 0.25 * (sector == "Catholic"))
  passmath <- factor(stats::rbinom(n, 1, pass_prob), levels = c(0, 1), labels = c("No", "Yes"))
  absence_rate <- exp(1.25 - 0.22 * ses - 0.014 * priorachieve + 0.28 * (minority == "Yes"))
  absences <- stats::rpois(n, lambda = pmax(0.05, absence_rate))
  # True district-level (Level 3) contextual predictor drawn after existing RNG
  # draws so all legacy columns and validation benchmark seeds stay bit-exact.
  district_ses_map <- tapply(school_meanses, school_to_district, mean) + stats::rnorm(n_districts, 0, 0.15)
  districtses <- round(as.numeric(district_ses_map[districtid]), 3)
  data.frame(
    studentid = sprintf("ST%04d", seq_len(n)),
    schoolid = factor(schoolid),
    districtid = factor(districtid),
    teacherid = factor(teacherid),
    mathscore = round(mathscore, 2),
    ses = round(ses, 3),
    priorachieve = round(priorachieve, 2),
    homework = round(homework, 1),
    meanses = round(meanses, 3),
    climate = round(climate, 3),
    schoolsize = size,
    districtses = districtses,
    passmath = passmath,
    absences = absences,
    sector = sector,
    female = female,
    minority = minority,
    stringsAsFactors = FALSE
  )
}

example_longitudinal <- function(n_students = 80, n_waves = 5, seed = 20260423) {
  if (!is.numeric(n_students) || length(n_students) != 1 || is.na(n_students) || n_students < 4) {
    stop("`n_students` must be a single number greater than or equal to 4.", call. = FALSE)
  }
  if (!is.numeric(n_waves) || length(n_waves) != 1 || is.na(n_waves) || n_waves < 3) {
    stop("`n_waves` must be a single number greater than or equal to 3.", call. = FALSE)
  }
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || is.na(seed))) {
    stop("`seed` must be a single numeric value or NULL.", call. = FALSE)
  }
  n_students <- as.integer(n_students)
  n_waves <- as.integer(n_waves)
  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit({
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = globalenv())
      } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      }
    }, add = TRUE)
    set.seed(seed)
  }
  n_schools <- max(8L, as.integer(ceiling(n_students / 8)))
  student_ids <- sprintf("P%03d", seq_len(n_students))
  student_school <- sprintf("S%02d", rep(seq_len(n_schools), length.out = n_students))
  student_treatment <- factor(sample(c("Control", "Intervention"), n_students, replace = TRUE))
  student_baseses <- round(stats::rnorm(n_students, 0, 0.8), 3)
  u0_person <- stats::rnorm(n_students, 0, 4.8)
  u1_person <- stats::rnorm(n_students, 0, 1.35)
  s0_school <- stats::rnorm(n_schools, 0, 2.2)
  names(s0_school) <- sprintf("S%02d", seq_len(n_schools))

  idx_student <- rep(seq_len(n_students), each = n_waves)
  wave <- rep(seq_len(n_waves) - 1L, times = n_students)
  n_obs <- length(wave)
  studyhours <- round(pmax(0, stats::rnorm(n_obs, 3.5 + 0.35 * student_baseses[idx_student] + 0.15 * wave, 1.1)), 2)
  reading <- 48 +
    2.6 * wave +
    1.15 * studyhours +
    2.4 * student_baseses[idx_student] +
    1.5 * (student_treatment[idx_student] == "Intervention") +
    1.1 * wave * (student_treatment[idx_student] == "Intervention") +
    s0_school[student_school[idx_student]] +
    u0_person[idx_student] +
    u1_person[idx_student] * wave +
    stats::rnorm(n_obs, 0, 3.4)
  proficient <- factor(
    stats::rbinom(n_obs, 1, stats::plogis(-1.1 + 0.08 * (reading - 50) + 0.25 * wave)),
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
  data.frame(
    obsid = sprintf("OBS%04d", seq_len(n_obs)),
    studentid = factor(student_ids[idx_student]),
    schoolid = factor(student_school[idx_student]),
    wave = wave,
    studyhours = studyhours,
    reading = round(reading, 2),
    proficient = proficient,
    treatment = student_treatment[idx_student],
    baseses = student_baseses[idx_student],
    stringsAsFactors = FALSE
  )
}

clean_imported_dataframe <- function(df, strings_as_factors = TRUE) {
  raw_labels <- vapply(df, function(col) {
    lbl <- attr(col, "label", exact = TRUE)
    if (is.character(lbl) && length(lbl) == 1) lbl else ""
  }, character(1))

  has_haven <- requireNamespace("haven", quietly = TRUE)
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled")) {
      val_labels <- attr(col, "labels", exact = TRUE)
      # If value labels exist and user wants categorical conversion, convert
      # labelled categories to R factors; otherwise strip wrapper to plain vector.
      if (!is.null(val_labels) && length(val_labels) > 0 && isTRUE(strings_as_factors) && has_haven) {
        df[[nm]] <- haven::as_factor(col, levels = "default")
      } else if (has_haven) {
        df[[nm]] <- haven::zap_labels(col)
      } else {
        attributes(col) <- NULL
        df[[nm]] <- col
      }
    }
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  clean_names <- make.names(names(df), unique = TRUE)
  names(raw_labels) <- clean_names
  names(df) <- clean_names

  for (nm in names(df)) {
    if (is.character(df[[nm]]) && isTRUE(strings_as_factors)) {
      df[[nm]] <- factor(df[[nm]])
    }
    if (nzchar(raw_labels[[nm]])) {
      attr(df[[nm]], "label") <- raw_labels[[nm]]
    }
  }
  attr(df, "variable_labels") <- raw_labels
  df
}

read_uploaded_datafile <- function(path, filename = basename(path), strings_as_factors = TRUE) {
  ext <- tolower(tools::file_ext(filename))
  imported <- if (ext %in% "csv") {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext %in% c("txt", "tsv")) {
    sep <- if (identical(ext, "tsv")) "\t" else ""
    utils::read.table(path, header = TRUE, sep = sep, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Install the readxl package to upload Excel files.", call. = FALSE)
    }
    readxl::read_excel(path)
  } else if (ext %in% c("sav", "por", "sas7bdat", "xpt", "dta")) {
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Install the haven package to upload SPSS, SAS, or Stata files.", call. = FALSE)
    }
    switch(
      ext,
      sav = haven::read_sav(path),
      por = haven::read_por(path),
      sas7bdat = haven::read_sas(path),
      xpt = haven::read_xpt(path),
      dta = haven::read_dta(path)
    )
  } else {
    stop("Unsupported file type. Use CSV, TSV/TXT, Excel, SPSS, SAS, or Stata files.", call. = FALSE)
  }
  clean_imported_dataframe(imported, strings_as_factors = strings_as_factors)
}
