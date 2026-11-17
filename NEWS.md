# mlmr 0.1.0

## Guided Multilevel Workflow and Robust Inference

- Added **Active Multilevel Model Blueprint** and **Dataset Variable Guide** on
  **Data > Overview** so users can immediately see the active Dependent Variable
  (Outcome $Y$), Multilevel Clustering Structure, Independent Variables (with
  level, centering, random-slope, and moderator roles), and live symbolic
  equations right from the home screen.
- Placed **Outcome distribution** and conditional **Link function** directly
  under the Dependent Variable selector on both the **Data** and **Model**
  sidebars, and replaced overlapping grouping dropdowns with an explicit 4-mode
  **Multilevel clustering design** selector (`two_level`, `three_level`,
  `longitudinal`, `crossed`) paired with dedicated Level-2 / Level-3 cluster
  selectors.
- Redesigned the **Model** tab into a numbered 4-step Equation Builder with
  inline predictor centering (`CWC`, `GMC`, uncentered), per-variable **Random
  slope (`+ u`)** checkboxes, dedicated **Cross-Level Slope Moderator**
  selectors, a visual within-level interaction picker, contextual advanced
  overrides inside Step 4, and a live pre-fit symbolic equation workspace
  powered by `mlm_spec_equations()`.
- Added exported inferential and diagnostic companions in `R/robust_inference.R`:
  - `mlm_robust_se()` for Huber-White cluster-robust sandwich standard errors
    (`CR1` small-sample correction and `CR0` asymptotic form);
  - `mlm_wald_test()` for joint multiparameter Wald $\chi^2$ hypothesis tests on
    fixed effects using model-based or cluster-robust covariance matrices;
  - `mlm_variance_tests()` for single-term random-effect likelihood-ratio
    $\chi^2$ tests with Stram-Lee boundary-mixture *p*-values;
  - `mlm_homogeneity_test()` for Bartlett's $H$ test of homogeneity of Level-1
    residual variance across clusters;
  - `mlm_population_average()` for unit-specific vs. Zeger population-average
    marginal coefficients and odds ratios in binary logit mixed models;
  - `mlm_residual_files()` for exporting Level-1 and cluster-level empirical
    Bayes residual tables.
- Upgraded `mlm_reliability()` via a Woodbury matrix formulation to compute
  empirical Bayes reliabilities ($\hat{\lambda}$) for **both** random intercepts
  and random slopes across clusters.
- Added `mlm_spec_equations()` to generate symbolic Level-1, Level-2, Level-3,
  Combined, and $\mathbf{T}$ equations directly from an `mlm_spec` before model
  estimation, and added `mlm_feature_matrix()` summarizing 25 package
  capabilities.

## Architecture and Single-Source Modernization

- Unified the package and Shiny application into modular single-source files
  under `R/` (`utils.R`, `data_hsb.R`, `spec_formula.R`, `readiness.R`,
  `fit_diagnostics.R`, `robust_inference.R`, `equations.R`,
  `tables_reporting.R`, `repro_code.R`, `validation_parity.R`, `app_ui.R`,
  `app_server.R`, `run_mlmr.R`), eliminating 5,100+ lines of duplicated code
  across `inst/app/` and retiring `dev/sync-app.R`.
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
- Added `example_longitudinal()` and a built-in longitudinal growth-curve
  dataset and template in the Shiny app.
- Added a Level-3 contextual predictor (`districtses`) to `example_hsb()` while
  preserving existing RNG draws and benchmark seeds bit-for-bit.
- Added `mlm_write_publication_bundle()` and upgraded the Shiny **Download
  Bundle** export into a complete `.zip` reproducibility archive containing the
  Quarto report, standalone R script, LaTeX file, HTML APA tables, Level-1 &
  cluster residual CSVs, checklist CSV, and software versions CSV.
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
- Escaped `|` characters inside `lme4` formulas in Markdown pipe tables, wrapped
  multi-line equations in `\begin{aligned}` for Quarto/MathJax, added
  `\usepackage{amsmath}` to LaTeX preambles, suppressed false Gaussian ICC
  calculations for GLMMs in `generate_repro_code()`, and used `deparse()` for
  all generated R string literals.
- Fixed unreadable dark-mode equation contrast (`.equation-rendered`) and
  restored visual hierarchy on `.btn-primary` buttons in `style.css`.
