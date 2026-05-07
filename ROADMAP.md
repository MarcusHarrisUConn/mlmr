# mlmr Roadmap

`mlmr` is currently a public beta package and Shiny app. The roadmap below is
intended to move the project from a reliable beta toward a CRAN-ready and
classroom/manuscript-ready production release.

## Public Beta Priorities

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
