mlm_supported_models <- function() {
  data.frame(
    Area = c(
      "Model families",
      "Nested structures",
      "Three-level structures",
      "Crossed random effects",
      "Multiple membership",
      "Repeated measures / longitudinal",
      "Random effects",
      "Centering",
      "Interactions",
      "Categorical predictors",
      "Denominator degrees of freedom",
      "Effect sizes and reliability",
      "Uploaded data",
      "Diagnostics",
      "Reporting exports",
      "Software citations"
    ),
    Status = c(
      "Supported with scope warnings",
      "Supported",
      "Supported",
      "Supported (advanced)",
      "Planned",
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Supported through R contrasts",
      "Supported (optional via lmerTest)",
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Supported"
    ),
    Scope = c(
      "Gaussian linear mixed models are the primary production path; binomial, Poisson, negative binomial, and Gamma GLMMs are available as advanced workflows.",
      "Two-level nested models are supported in the app and backend.",
      "Three-level nested models are supported with per-level random intercepts, slopes, and pi/beta/gamma equations.",
      "Separate lme4 random-effect blocks can be specified with per-factor random slopes and mixed-model equation rendering.",
      "Weighted membership models are outside the current lme4-backed production scope.",
      "Longitudinal growth-curve models are supported via built-in templates, example_longitudinal(), and wave-by-person random slopes.",
      "Random intercepts, per-grouping-factor random slopes, and correlated or independent random-effect structures are supported.",
      "No centering, grand-mean centering, and cluster-mean centering (at any chosen grouping level) are supported for numeric predictors.",
      "Fixed interactions and cross-level interaction terms are supported with main-effect readiness checks.",
      "Factors, character variables, and labelled SPSS/SAS/Stata categories are modeled using R's contrast system; dummy-coding summaries are reported.",
      "Gaussian models support large-sample normal tests by default and Satterthwaite or Kenward-Roger degrees of freedom when lmerTest/pbkrtest are installed.",
      "Gaussian ICCs, Nakagawa marginal/conditional R^2 (LMMs and GLMMs), and HLM-style random-intercept reliability (lambda) are computed.",
      "CSV, TSV/TXT, Excel, SPSS, SAS, and Stata files are supported with automatic haven_labelled handling when optional readers are installed.",
      "Convergence messages, singular-fit checks, gradients, GLMM overdispersion, complete-case residual plots, and caterpillar plots are reported.",
      "APA tables, raw LaTeX, equations, Quarto reports, reproducible R scripts, and .zip publication bundles are supported.",
      "R/package version tables, APA software statements, and optional papaja citation code are supported."
    ),
    User_responsibility = c(
      "Confirm distributional assumptions and link functions.",
      "Confirm the grouping IDs correctly represent the design.",
      "Confirm enough units exist at each higher level for stable estimation.",
      "Confirm the design is truly crossed and not a miscoded hierarchy.",
      "Use specialized methods outside mlmr for weighted multiple-membership models.",
      "Confirm time is coded appropriately and evaluate residual autocorrelation if waves are closely spaced.",
      "Inspect convergence, singular fits, and whether the random-effects structure is supported by the data.",
      "Match centering choices to within-group, between-group, or contextual research questions.",
      "Include lower-order main effects and interpret lower-order terms conditionally when interactions are present.",
      "Confirm reference categories and contrasts before reporting.",
      "Select Satterthwaite or Kenward-Roger degrees of freedom for confirmatory small-cluster Gaussian inference.",
      "Report scale assumptions when interpreting latent- or observation-scale R^2 for non-Gaussian outcomes.",
      "Check variable types, missing data, and imported value labels before fitting.",
      "Treat diagnostics as evidence for model revision, not as automatic pass/fail gates.",
      "Review all manuscript text, labels, notation, and inferential method choices before submission.",
      "Confirm package citations meet the target journal or style guide."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

mlm_hlm8_parity <- function() {
  data.frame(
    HLM8_workflow = c(
      "Unconditional/null model",
      "Random-intercept model",
      "Random-slope model",
      "Cross-level interaction",
      "Two-level nested model",
      "Three-level nested model",
      "Longitudinal growth-curve model",
      "Cross-classified random effects",
      "Centering decisions",
      "Small-sample denominator df",
      "Binary outcome HGLM",
      "Count outcome HGLM",
      "Model comparison (LRT / AIC / BIC)",
      "Variance components, ICCs, and R^2",
      "Reliability summaries (lambda)",
      "Equation display",
      "Diagnostics",
      "Publication tables",
      "Reproducible syntax export",
      "Software citation and methods text",
      "Missing-data review",
      "Multiple-imputation pooling",
      "Fully weighted multiple membership"
    ),
    mlmr_status = c(
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Supported (advanced)",
      "Supported",
      "Supported",
      "Experimental",
      "Experimental",
      "Supported",
      "Supported",
      "Supported for Gaussian LMMs",
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Supported",
      "Planned",
      "Out of current scope"
    ),
    Evidence = c(
      "Model template and formula generation support intercept-only models.",
      "Model template and lme4 random-intercept syntax are available.",
      "Per-grouping-factor random slopes can be selected for numeric fixed predictors.",
      "Fixed interactions are supported, with readiness checks for omitted main effects.",
      "Grouping factors and random-effect blocks support standard two-level models.",
      "Additional grouping factors support three-level nested models with pi/beta/gamma equations.",
      "example_longitudinal() and longitudinal growth preset support wave-within-person trajectories.",
      "Crossed random-effect blocks fit with lme4 and render in mixed-model equation notation.",
      "No centering, grand-mean centering, and cluster-mean centering are available.",
      "Optional Satterthwaite and Kenward-Roger degrees of freedom are available via lmerTest/pbkrtest.",
      "Advanced mode fits binomial mixed models through lme4::glmer() and reports odds ratios.",
      "Advanced mode fits Poisson and negative-binomial mixed models and reports incidence rate ratios.",
      "Stored model history checks sample/outcome compatibility and computes ML-refit LRT tables.",
      "Variance components, Gaussian ICCs, and Nakagawa marginal/conditional R^2 are reported.",
      "mlm_reliability() computes empirical Bayes random-intercept reliability across clusters.",
      "Level-by-level, combined, and row/column-labeled Tau matrix equations are generated.",
      "Convergence, singular fit, gradients, overdispersion, NA-safe residual plots, and group/slope caterpillar plots are available.",
      "APA fixed effects, dummy coding, variance components, ICC, R^2, and software tables are available.",
      "Downloadable R scripts, Quarto reports, LaTeX, and .zip publication bundles reproduce fitted models.",
      "Publication bundle archive includes methods text, reproducibility checklist, and software statements.",
      "Missingness summaries, pattern tables, and mice imputation scaffold code are available.",
      "Scaffold code exists, but pooled mixed-model reporting inside the GUI is planned.",
      "Weighted multiple-membership models require methods outside the current lme4-backed production path."
    ),
    Next_step = c(
      "Maintain deterministic benchmark coverage.",
      "Maintain deterministic benchmark coverage.",
      "Continue refining singular-fit simplification hints.",
      "Add optional simple-slopes interaction plotting.",
      "Expand classroom case-study examples.",
      "Add three-level longitudinal examples.",
      "Add optional residual autocorrelation guidance.",
      "Add cross-classified variance-partitioning examples.",
      "Keep within- vs contextual-effect notes aligned with centering choices.",
      "Add optional parametric bootstrap intervals.",
      "Expand binary calibration and separation diagnostics.",
      "Expand zero-inflation screening diagnostics.",
      "Support side-by-side multi-model APA comparison tables.",
      "Add confidence intervals for ICC and R^2 via bootstrap.",
      "Extend reliability summaries to random slopes.",
      "Allow custom variable display labels in equations.",
      "Add cluster-level Cook's distance / influence diagnostics.",
      "Add native .docx table generation.",
      "Maintain round-trip code execution checks in benchmarks.",
      "Expand journal-specific methods templates.",
      "Add visual missing-data pattern plots.",
      "Implement and validate Rubin's rules pooling for mixed models.",
      "Document specialized R packages (e.g., brms, lmerMultiMember)."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

benchmark_specs <- function(dat, dat_long = NULL) {
  if (is.null(dat_long)) {
    dat_long <- example_longitudinal(n_students = 32, n_waves = 4, seed = 20260423)
  }
  list(
    null = mlm_spec(
      outcome = "mathscore",
      fixed = list(),
      grouping = list(schoolid = "schoolid"),
      random = list(schoolid = list(intercept = TRUE, slopes = character(), correlation = TRUE)),
      data = dat
    ),
    random_intercept = mlm_spec(
      outcome = "mathscore",
      fixed = list(ses = list(center = "CWC"), meanses = list(center = "GMC")),
      grouping = list(schoolid = "schoolid"),
      random = list(schoolid = list(intercept = TRUE, slopes = character(), correlation = TRUE)),
      data = dat
    ),
    random_slope = mlm_spec(
      outcome = "mathscore",
      fixed = list(ses = list(center = "CWC"), meanses = list(center = "GMC")),
      grouping = list(schoolid = "schoolid"),
      random = list(schoolid = list(intercept = TRUE, slopes = "ses", correlation = TRUE)),
      data = dat
    ),
    cross_level = mlm_spec(
      outcome = "mathscore",
      fixed = list(ses = list(center = "CWC"), sector = list(center = "none")),
      grouping = list(schoolid = "schoolid"),
      random = list(schoolid = list(intercept = TRUE, slopes = "ses", correlation = TRUE)),
      interactions = list(c("ses", "sector")),
      data = dat
    ),
    three_level = mlm_spec(
      outcome = "mathscore",
      fixed = list(ses = list(center = "CWC"), meanses = list(center = "GMC")),
      grouping = list(schoolid = "schoolid", districtid = "districtid"),
      random = list(
        schoolid = list(intercept = TRUE, slopes = "ses", correlation = TRUE),
        districtid = list(intercept = TRUE, slopes = character(), correlation = TRUE)
      ),
      data = dat
    ),
    crossed = mlm_spec(
      outcome = "mathscore",
      fixed = list(ses = list(center = "CWC")),
      grouping = list(schoolid = "schoolid", teacherid = "teacherid"),
      structure = "crossed",
      random = list(
        schoolid = list(intercept = TRUE, slopes = character(), correlation = TRUE),
        teacherid = list(intercept = TRUE, slopes = character(), correlation = TRUE)
      ),
      data = dat
    ),
    longitudinal = mlm_spec(
      outcome = "reading",
      fixed = list(wave = list(center = "none"), treatment = list(center = "none")),
      grouping = list(studentid = "studentid"),
      random = list(studentid = list(intercept = TRUE, slopes = "wave", correlation = TRUE)),
      interactions = list(c("wave", "treatment")),
      data = dat_long
    ),
    logistic_glmm = mlm_spec(
      outcome = "passmath",
      distribution = "binomial",
      link = "logit",
      fixed = list(ses = list(center = "CWC")),
      grouping = list(schoolid = "schoolid"),
      random = list(schoolid = list(intercept = TRUE, slopes = character(), correlation = TRUE)),
      data = dat
    )
  )
}

verify_repro_roundtrip <- function(result) {
  code <- generate_repro_code(result)
  cutoff <- grep("Multiple Imputation Scaffold", code, fixed = TRUE)
  if (length(cutoff)) code <- code[seq_len(cutoff[[1]] - 2L)]
  env <- new.env(parent = globalenv())
  # Strip derived centered columns so the exported script recreates them from raw data.
  raw_cols <- unique(c(model_variables(result$spec)))
  raw_cols <- intersect(raw_cols, names(result$data))
  env$dat <- result$data[, raw_cols, drop = FALSE]
  ok <- tryCatch(
    {
      utils::capture.output(suppressMessages(suppressWarnings(eval(parse(text = code), envir = env))))
      inherits(env$fit, "merMod") &&
        max(abs(lme4::fixef(env$fit) - lme4::fixef(result$fit))) < 1e-5
    },
    error = function(e) FALSE
  )
  isTRUE(ok)
}

mlm_validation_benchmarks <- function(run = FALSE, seed = 20260423, tolerance = 0.5) {
  dat <- example_hsb(n_schools = 8, min_students = 8, max_students = 10, seed = seed)
  dat_long <- example_longitudinal(n_students = 32, n_waves = 4, seed = seed)
  expected <- data.frame(
    Benchmark = c(
      "null", "random_intercept", "random_slope", "cross_level",
      "three_level", "crossed", "longitudinal", "logistic_glmm"
    ),
    `Expected Formula` = c(
      "mathscore ~ 1 + (1 | schoolid)",
      "mathscore ~ ses_CWC + meanses_GMC + (1 | schoolid)",
      "mathscore ~ ses_CWC + meanses_GMC + (1 + ses_CWC | schoolid)",
      "mathscore ~ ses_CWC + sector + ses_CWC:sector + (1 + ses_CWC | schoolid)",
      "mathscore ~ ses_CWC + meanses_GMC + (1 + ses_CWC | schoolid) + (1 | districtid)",
      "mathscore ~ ses_CWC + (1 | schoolid) + (1 | teacherid)",
      "reading ~ wave + treatment + wave:treatment + (1 + wave | studentid)",
      "passmath ~ ses_CWC + (1 | schoolid)"
    ),
    `Expected AIC` = c(566.8003, 537.3551, 540.7158, 539.3033, 542.7158, 545.5367, 808.3674, 102.4937),
    ref_b0 = c(73.8400, 73.7158, 73.7166, 79.6538, 73.7166, 74.1172, 53.3432, 0.6003),
    ref_se0 = c(6.5338, 5.7654, 5.7587, 9.4033, 5.7587, 6.6163, 1.4229, 0.2853),
    ref_tau0 = c(333.3955, 259.8253, 259.5735, 347.8504, 259.5735, 8.2090, 36.7833, 0.1534),
    ref_icc1 = c(0.8147, 0.8217, 0.8305, 0.8695, 0.8305, 0.0207, 0.7247, NA_real_),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  specs <- benchmark_specs(dat, dat_long)
  rows <- lapply(names(specs), function(name) {
    spec <- specs[[name]]
    formula_text <- paste(deparse(build_formula(spec), width.cutoff = 500), collapse = " ")
    readiness <- model_readiness_table(spec$data, spec)
    expected_row <- expected[expected$Benchmark == name, , drop = FALSE]
    row <- data.frame(
      Benchmark = name,
      Formula = formula_text,
      `Expected Formula` = expected_row$`Expected Formula`,
      `Formula Match` = identical(formula_text, expected_row$`Expected Formula`),
      `Readiness Stops` = sum(readiness$Status == "Stop"),
      `Fit Tested` = "No",
      AIC = NA_real_,
      `Expected AIC` = expected_row$`Expected AIC`,
      `AIC Difference` = NA_real_,
      `Estimate Diff` = NA_real_,
      `SE Diff` = NA_real_,
      `VarCorr Diff` = NA_real_,
      `ICC Diff` = NA_real_,
      `Repro Code Match` = "Not run",
      `Validation Status` = if (identical(formula_text, expected_row$`Expected Formula`) && sum(readiness$Status == "Stop") == 0) "Formula validated" else "Review",
      `Fixed Effects` = length(names(spec$fixed)),
      `Random Blocks` = length(names(spec$random)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (isTRUE(run)) {
      fit <- suppressWarnings(mlm_fit(spec, REML = TRUE, optimizer = "bobyqa", maxfun = 10000))
      fe <- fixed_effects_table(fit)
      vc <- variance_table(fit$fit)
      icc <- icc_table(fit$fit)
      repro_ok <- verify_repro_roundtrip(fit)
      row$`Fit Tested` <- "Yes"
      row$AIC <- stats::AIC(fit$fit)
      row$`AIC Difference` <- abs(row$AIC - row$`Expected AIC`)
      row$`Estimate Diff` <- round(abs(fe$Estimate[[1]] - expected_row$ref_b0), 5)
      row$`SE Diff` <- round(abs(fe$`Std. Error`[[1]] - expected_row$ref_se0), 5)
      row$`VarCorr Diff` <- round(abs(vc$vcov[[1]] - expected_row$ref_tau0), 4)
      row$`ICC Diff` <- if (is.na(expected_row$ref_icc1)) {
        if (nrow(icc) == 0L) 0 else NA_real_
      } else if (nrow(icc) > 0L) {
        round(abs(icc$icc[[1]] - expected_row$ref_icc1), 4)
      } else {
        NA_real_
      }
      row$`Repro Code Match` <- if (repro_ok) "Yes" else "No"
      est_ok <- is.finite(row$`Estimate Diff`) && row$`Estimate Diff` <= max(tolerance, 0.05) &&
        is.finite(row$`SE Diff`) && row$`SE Diff` <= max(tolerance, 0.05) &&
        is.finite(row$`VarCorr Diff`) && row$`VarCorr Diff` <= max(tolerance * 2, 1.0) &&
        !is.na(row$`ICC Diff`) && row$`ICC Diff` <= 0.02
      row$`Validation Status` <- if (
        isTRUE(row$`Formula Match`) &&
          row$`Readiness Stops` == 0 &&
          is.finite(row$`AIC Difference`) &&
          row$`AIC Difference` <= tolerance &&
          est_ok &&
          repro_ok
      ) "Validated" else "Review"
    }
    row
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

mlm_validation_report <- function(seed = 20260423, tolerance = 0.5) {
  dat <- example_hsb(n_schools = 8, min_students = 8, max_students = 10, seed = seed)
  dat_long <- example_longitudinal(n_students = 32, n_waves = 4, seed = seed)
  specs <- benchmark_specs(dat, dat_long)
  add_benchmark <- function(tab, name) {
    if (!nrow(tab)) return(tab)
    data.frame(Benchmark = name, tab, check.names = FALSE, stringsAsFactors = FALSE)
  }
  fits <- lapply(specs, function(spec) {
    suppressWarnings(mlm_fit(spec, REML = TRUE, optimizer = "bobyqa", maxfun = 10000))
  })
  # Standardize fixed-effects columns across Gaussian and GLMM rows when binding:
  fixed_list <- lapply(names(fits), function(name) {
    tab <- apa_fixed_table(fits[[name]], exponentiate = FALSE)
    add_benchmark(tab, name)
  })
  fixed <- do.call(rbind, fixed_list)
  variance <- do.call(rbind, lapply(names(fits), function(name) add_benchmark(variance_table(fits[[name]]$fit), name)))
  icc <- do.call(rbind, lapply(names(fits), function(name) add_benchmark(icc_table(fits[[name]]$fit), name)))
  r2 <- do.call(rbind, lapply(names(fits), function(name) add_benchmark(mlm_r2(fits[[name]]), name)))
  diagnostics <- do.call(rbind, lapply(names(fits), function(name) add_benchmark(mlm_diagnostics(fits[[name]]$fit), name)))
  row.names(fixed) <- row.names(variance) <- row.names(icc) <- row.names(r2) <- row.names(diagnostics) <- NULL
  list(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    seed = seed,
    tolerance = tolerance,
    benchmarks = mlm_validation_benchmarks(run = TRUE, seed = seed, tolerance = tolerance),
    fixed_effects = fixed,
    variance_components = variance,
    icc = icc,
    r2 = r2,
    diagnostics = diagnostics
  )
}
