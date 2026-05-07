# mlmr 0.1.0

## Public beta preparation

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
