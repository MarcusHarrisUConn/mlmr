# mlmr Roadmap

`mlmr` is currently a public beta package and Shiny app. The roadmap below is
intended to move the project from a reliable beta toward a CRAN-ready and
classroom/manuscript-ready production release.

## Public Beta Priorities

- Maintain an HLM 8 parity checklist through `mlm_hlm8_parity()` so users can
  see which HLM-style workflows are supported, experimental, planned, or out of
  scope.
- Expand `mlm_validation_benchmarks()` and `mlm_validation_report()` beyond the
  current expected formula, AIC, fitted-output, and diagnostic summaries into a
  gold-standard validation suite with stored reference estimates, standard
  errors, variance components, ICCs, and exported-code checks.
- Continue hardening formula generation for nested, crossed, longitudinal, and
  GLMM designs.
- Expand validation messages for uploaded data, selected variables, grouping
  IDs, and declared predictor levels.
- Expand tests for edge cases in centering, formulas, equations, Tau matrices,
  APA tables, diagnostics, GLMMs, and exports.
- Add Quarto tutorials for two-level models, three-level models, centering,
  cross-level interactions, diagnostics, and manuscript reporting.
- Keep the `pkgdown` documentation site synchronized with package releases.
- Continue separating reusable package backend functions from Shiny-only
  presentation code.

## Modeling Features

- Expand crossed random-effects templates and examples.
- Add longitudinal/repeated-measures templates.
- Add GLMM workflows for binary, count, and skewed outcomes.
- Add model-comparison workflows with clearer REML/ML guidance.
- Add richer missing-data workflows, including multiple-imputation examples.

## Reporting Features

- Improve manuscript-ready Quarto report export.
- Expand the publication bundle with one-click archive output and reviewer-
  facing reproducibility appendices.
- Add copy-ready APA table exports for Word, HTML, and LaTeX.
- Add clearer equation annotations for fixed effects, random effects, and Tau
  matrix parameters.
- Add optional plain-language interpretation panels for teaching and reporting.

## CRAN Preparation

- Confirm package name availability immediately before submission.
- Review exported function names and documentation one final time.
- Keep examples fast, reliable, and CRAN-safe.
- Run `R CMD check --as-cran` locally and on Windows, macOS, Linux, and R-devel.
- Confirm all vignettes are lightweight enough for CRAN checks.
- Confirm an installed-package smoke test launches `mlmr::run_mlmr()`.

## Software Paper Preparation

- Keep the draft paper in `paper/` excluded from the CRAN source package.
- Frame the statement of need around a free, reproducible, GUI-first
  alternative to expensive HLM-style software.
- Add a case study showing how a student moves from data to model equations,
  APA tables, diagnostics, reproducible R code, and a manuscript appendix.
- Before submission, document validation evidence from
  `mlm_validation_benchmarks()` and public beta feedback.
