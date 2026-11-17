# mlmr 0.1.0

## Architecture and Single-Source Modernization

- Unified the package and Shiny application into modular single-source files
  under `R/` (`utils.R`, `data_hsb.R`, `spec_formula.R`, `readiness.R`,
  `fit_diagnostics.R`, `equations.R`, `tables_reporting.R`, `repro_code.R`,
  `validation_parity.R`, `app_ui.R`, `app_server.R`, `run_mlmr.R`), eliminating
  5,100+ lines of duplicated code across `inst/app/` and retiring
  `dev/sync-app.R`.
- `run_mlmr()` gains a `host` argument (defaulting to `"127.0.0.1"`) and
  launches the package-backed Shiny app directly; `shiny::runApp(".")` continues
  to work from a local repository checkout.

## Statistical, Diagnostic, and Reporting Improvements

- Added optional Satterthwaite and Kenward-Roger denominator degrees of freedom
  (`df_method = c("none", "satterthwaite", "kenward-roger")`) to `mlm_fit()` via
  `lmerTest` and `pbkrtest`, propagating `df` and *t*-based confidence intervals
  through APA tables, methods text, reproducibility checks, and exported R code.
- Added `mlm_r2()` to compute marginal and conditional $R^2$ for Gaussian and
  generalized mixed models (Nakagawa, Johnson, & Schielzeth, 2017; Johnson,
  2014).
- Added `mlm_reliability()` to compute HLM-style empirical Bayes
  random-intercept reliabilities across clusters for Gaussian models (Raudenbush
  & Bryk, 2002).
- Added `example_longitudinal()` and a built-in longitudinal growth-curve
  dataset and template in the Shiny app.
- Added a true Level-3 contextual predictor (`districtses`) to `example_hsb()`
  while preserving existing RNG draws and benchmark seeds bit-for-bit.
- Added `mlm_write_publication_bundle()` and upgraded the Shiny **Download
  Bundle** export into a complete `.zip` reproducibility archive containing the
  Quarto report, standalone R script, LaTeX file, HTML APA tables, checklist
  CSV, and software versions CSV.
- Expanded `mlm_validation_benchmarks()` to 8 deterministic models (`null`,
  `random_intercept`, `random_slope`, `cross_level`, `three_level`, `crossed`,
  `longitudinal`, and `logistic_glmm`), verifying AICs, fixed-effect estimates,
  standard errors, variance components, ICCs, and automated round-trip R script
  execution.

## Bug Fixes and UX Hardening

- Fixed `model_writeup()` reporting `"maximum likelihood"` for REML models due
  to `isTRUE()` on integer `devcomp$dims[["REML"]]`; write-ups now use
  `lme4::isREML(fit)` and follow APA 7th *p*-value formatting (`p < .001`
  without `=`, exact values without leading zeros).
- Fixed a row-count crash on **Results > Diagnostics** when fitted data
  contained `NA` values, mapped binary factor outcomes to `0/1` probabilities on
  response-scale plots, and added grouping-factor and random-term selectors to
  the random-effects caterpillar plot so multi-level models no longer mix
  grouping levels or fail on overlapping IDs.
- Fixed **Results > Compare** likelihood-ratio tests (`anova()` named-list
  dispatch failure), added outcome/sample-size compatibility checks, and added a
  **Clear Comparison History** button.
- Fixed custom CSV and SPSS/SAS/Stata uploads triggering missing-variable errors
  from stale HSB interaction defaults; `haven_labelled` categorical columns now
  convert cleanly to factors with variable labels preserved in **Data > Roles**.
- Redesigned per-grouping-factor random-effects state on the **Model** tab so
  random slopes and correlation toggles persist independently across grouping
  factors, restored the live pre-fit Tau covariance structure preview, and
  removed dead UI/server controls.
- Tuned the flagship **Fit Example Model** workflow so it converges without
  singularity warnings out of the box and synchronizes Model tab controls.
- Escaped `|` characters inside `lme4` formulas in Markdown pipe tables, wrapped
  multi-line equations in `\begin{aligned}` for Quarto/MathJax, added
  `\usepackage{amsmath}` to LaTeX preambles, suppressed false Gaussian ICC
  calculations for GLMMs in `generate_repro_code()`, and used `deparse()` for
  all generated R string literals.
- Fixed unreadable dark-mode equation contrast (`.equation-rendered`) and
  restored visual hierarchy on `.btn-primary` buttons in `style.css`.
- Expanded uploaded-data readiness checks for interaction components missing
  main effects, constant/near-zero variance predictors, high-cardinality
  categorical predictors, continuous numeric grouping IDs, and multi-level
  within-group variation.
- Added row/column headers to rendered Tau covariance grids, added a `Role`
  column (`Fixed only` vs `Random`) to the equation Coefficient Key, and added
  copy-to-clipboard buttons across reporting panels.
