# mlmr Roadmap

`mlmr` is currently an early public prototype. The roadmap below is intended to
move the project from a working Shiny app toward a reliable public beta and,
eventually, a CRAN-ready R package.

## Public Beta Priorities

- Harden formula generation for nested, crossed, and multiple-membership
  designs.
- Improve validation messages for uploaded data and selected variables.
- Add unit tests for centering, model formulas, equations, Tau matrices, APA
  tables, diagnostics, and exports.
- Add Quarto tutorials for getting started, two-level models, three-level
  models, centering, and equations/reporting.
- Build a `pkgdown` documentation site for GitHub Pages.
- Separate package backend functions from Shiny-only presentation code.

## Modeling Features

- Expand crossed random-effects templates.
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

- Replace placeholder maintainer email in `DESCRIPTION`.
- Review exported function names and documentation.
- Add examples that run quickly and reliably.
- Run `R CMD check --as-cran` on Windows, macOS, and Linux.
- Confirm all vignettes are lightweight enough for CRAN checks.
