# mlmr 0.1.0

## Correctness fixes

- Exported reproducible R code now runs as written. Centering statements refer
  to columns of `dat`, the script selects the analysis sample before centering,
  and refitting reproduces the model estimated by `mlm_fit()`.
- Level-by-level equations now place predictors at the level where they vary.
  Higher-level predictors appear in the intercept equation and cross-level
  interactions appear in the equation for the slope they moderate, instead of
  every term being written as a Level-1 predictor.
- Equations are built from the fitted coefficients, so each dummy code of a
  categorical predictor has its own coefficient and random-effect indices in
  the Tau matrices match the equations. Previously the two could disagree when
  a factor with more than two categories preceded a random slope.
- Generalized models are written for the linear predictor with the link
  function shown, without a Level-1 residual term.
- Centering now uses the analysis sample. Grand and cluster means are computed
  from rows that are complete on all model variables, so centered predictors
  have mean zero among the observations the model uses. Results are unchanged
  for complete data.
- Methods text, the reproducibility checklist, and exported code now read the
  estimator from the fitted model and the optimizer settings recorded by
  `mlm_fit()`, so they cannot report REML for a model estimated with ML or a
  different `maxfun` than the one used.
- The installed Shiny app was running an older copy of the modeling code than
  the package. The copies are synchronized again, `dev/sync-app.R` refreshes
  them, and a test fails if they drift.
- A random-effect block with neither an intercept nor slopes no longer produces
  the invalid term `(0 | group)`.

## Improvements

- `mlm_fit()` returns an object of class `"mlm_result"` with a `print()` method
  and the estimation settings; `"mlm_spec"` objects also print a summary.
- `mlm_fit()` explains specifications it cannot fit: centering a categorical
  predictor, an unknown centering method, a random slope for a variable that is
  not a fixed predictor, a missing data frame, or no random effects. The same
  problems appear as stop items in the app's model-readiness table.
- Three-level nested models are written with pi, beta, and gamma coefficients.
  Crossed designs are written as a single mixed-model equation.
- `mlm_latex_equations()` also returns a coefficient key linking each symbol to
  its estimate, the notation used, and an explanatory note. The app shows the
  key on the Equations tab and reports include it.
- `mlm_apa_tables()` gains `exponentiate = TRUE` for odds ratios (logit link)
  and incidence rate ratios or `exp(b)` (log link) with confidence intervals.
  The app shows these columns for generalized models.
- Cluster-mean centering can target a specific grouping factor with
  `list(center = "CWC", group = "districtid")`.
- Variable names that are not syntactic R names are quoted in formulas and
  handled in generated code.
- The readiness table checks whether grouping IDs form the declared structure
  and warns when lower-level IDs are reused across higher-level units, which
  makes lme4 treat a nested design as crossed.
- LaTeX tables escape `#`, `$`, `{`, `}`, `^`, `~`, and `>` in addition to the
  characters handled before.
- Added a `testthat` suite covering specification, centering, fitting,
  equations, reporting, readiness checks, and app synchronization.

## Public beta preparation

- Added HLM-style feature parity tracking with `mlm_hlm8_parity()`, plus a
  dedicated vignette describing parity and validation evidence.
- Added deterministic validation benchmarks and a reviewer-facing validation
  report through `mlm_validation_benchmarks()` and `mlm_validation_report()`.
- Added publication-readiness helpers for methods text, reproducibility
  checklists, and publication bundles.
- Added reviewer-facing inference guardrails: GLMM-specific report text,
  visible Wald-inference cautions, Gaussian-only classical ICC output, stricter
  family/link validation, and random-slope/binomial-sparsity readiness checks.
- Restored rendered equation displays in the Shiny app with local HTML/CSS
  rendering for Greek symbols, subscripts, multi-line equations, and Tau
  matrices while keeping raw LaTeX available for export.
- Made the getting-started vignette execute a lightweight package-scope example
  during vignette builds while keeping install and app-launch chunks unevaluated.
- Added software-article scaffolding, citation metadata, and machine-readable
  software metadata outside the CRAN source package.
- Removed external MathJax/CDN loading from the Shiny app so the installed app
  does not depend on a remote JavaScript service at runtime.
- Added a publication-readiness checklist for the next CRAN and beta-testing
  milestones.
- Strengthened validation for example-data generation and model specification
  when selected variables are missing from the data.
- Expanded backend tests for manuscript-facing outputs, including APA tables,
  raw LaTeX, wrapped equations, Quarto report text, software reporting, and
  app/backend synchronization.
- Expanded the GitHub bug-report template so testers can report model
  structure, centering, interactions, and export-specific issues more clearly.
- Added a local publication-readiness rehearsal script that checks app/backend
  copy synchronization, public documentation for local paths, release-file
  presence, and source tarball contents.
- Added GitHub issue-template links to the documentation and beta testing guide.
- Published GitHub prerelease `v0.1.0-alpha.2` for structured beta testing.
- Added a structured beta testing guide for early users and linked it from the
  documentation site.
- Expanded the GitHub feedback template so testers report model structure,
  exported-code checks, and app section details.
- Exported core backend helpers for model specification, formula generation,
  fitting, equations, APA tables, and software/package reporting.
- Added `mlm_supported_models()` and a "Supported Models and Production Scope"
  vignette so users can see supported, experimental, planned, and out-of-scope
  features before using the app in real analyses.
- Added APA-style software and R package reporting for reproducible manuscripts,
  including Shiny app display, Quarto report output, and LaTeX export.
- Added optional `papaja` citation code generation for users who want APA-style
  R/package citations and BibTeX references in R Markdown manuscripts.
- Added package citation metadata and expanded multi-platform check preparation.
- Polished README installation, demo, documentation, and feedback guidance.
- Added package-level documentation and runnable interactive example for
  `run_mlmr()`.
- Added CRAN submission comments and release checklist scaffolding.
- Added a GitHub Actions workflow for building and publishing the `pkgdown`
  documentation site.
- Kept root development app files out of the built package; the installed Shiny
  app is shipped through `inst/app/`.

## Development version

- Added the initial Shiny app for mixed-effects and multilevel model building.
- Added the HSB-style example data workflow.
- Added model specification, formula generation, centering, fitting, diagnostics,
  APA-style tables, model equations, Tau matrix display, and reproducible code
  export.
- Added Quarto, R script, LaTeX, HTML, and Word-compatible export scaffolds.
- Added package metadata, GitHub URLs, roadmap, and initial backend tests.
- Added model-readiness checks before fitting custom models.
- Added direct data upload support for CSV, TSV/TXT, Excel, SPSS, SAS, and
  Stata files.
- Added loading indicators for slower diagnostics plots.
