# mlmr

<!-- badges: start -->
[![R CMD check](https://github.com/MarcusHarrisUConn/mlmr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MarcusHarrisUConn/mlmr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`mlmr` is an open-source R package and Shiny app for fitting, understanding,
and reporting mixed-effects and multilevel models. It is designed for
researchers, instructors, graduate students, and applied analysts who want a
guided model-building workflow without giving up transparent R code.

The app uses `lme4` under the hood and helps users move from data, grouping
structure, centering decisions, fixed effects, random effects, and interactions
to APA-style tables, level-by-level equations, combined equations, Tau
variance-covariance displays, diagnostics, and reproducible exports.

## Why Use mlmr?

Many researchers learn multilevel modeling through level-based software
workflows, but later need reproducible R syntax for manuscripts, theses,
teaching, and open science. `mlmr` is meant to bridge those worlds:

- model setup feels familiar to users who think in Level 1, Level 2, and Level
  3 terms;
- fitted models are still ordinary `lme4` models;
- centering, dummy coding, interactions, and random-effects choices are made
  explicit;
- results are paired with reproducible R code and manuscript-ready LaTeX.

## Current Features

- Guided Shiny interface for two- and three-level nested model workflows
- User-declared Level 1, Level 2, and Level 3 predictor selection
- Random intercept and random slope models
- Correlated and independent random-effects structures
- Grand-mean and cluster-mean centering controls
- Interaction and cross-level interaction support
- Built-in HSB-style example data with preset model choices
- Upload support for CSV, TSV/TXT, Excel, SPSS, SAS, and Stata files
- Model-readiness checks before fitting custom models
- APA-style fixed-effects, variance components, ICC, and dummy-coding tables
- Level-by-level equations, combined equations, and Tau matrix displays
- Diagnostics, model comparison tools, and convergence guidance
- Reproducible R code, Quarto report, HTML, Word-compatible, and LaTeX exports

## Installation

`mlmr` is currently available from GitHub:

```r
install.packages("pak")
pak::pak("MarcusHarrisUConn/mlmr")
```

You can also install with `remotes`:

```r
install.packages("remotes")
remotes::install_github("MarcusHarrisUConn/mlmr")
```

## Launch the App

```r
mlmr::run_mlmr()
```

The app opens with a built-in HSB-style example so users can test the complete
workflow before uploading their own data.

During local development, run the app directly from the project folder:

```r
shiny::runApp(".")
```

## Five-Minute Demo

1. Launch the app with `mlmr::run_mlmr()`.
2. Keep the built-in example data selected on the **Data** tab.
3. Review grouping factors, missingness, and variable roles.
4. Open **Model** and inspect the outcome, declared predictor levels,
   centering, interactions, and random-effects choices.
5. Click **Fit Example Model**.
6. Review **Results > Tables**, **Results > Equations**, and
   **Results > Diagnostics**.
7. Open **Report & Code** to export reproducible R code, raw LaTeX, APA tables,
   and a Quarto-ready report.

## Documentation

The package includes vignettes for getting started and for equations/reporting:

```r
vignette("getting-started", package = "mlmr")
vignette("equations-and-reporting", package = "mlmr")
```

A `pkgdown` documentation site is planned for GitHub Pages so users can browse
tutorials, reference documentation, screenshots, and beta feedback instructions
without installing the package first.

## Public Beta Feedback

`mlmr` is ready for structured demo testing and early feedback. If you try the
app, please open a GitHub issue with comments about the model-building workflow,
equations, APA tables, diagnostics, generated code, or uploaded-data experience:

<https://github.com/MarcusHarrisUConn/mlmr/issues>

Helpful feedback includes:

- the model structure you tried;
- whether you used the built-in example or uploaded data;
- screenshots of confusing output;
- generated code if reproducibility was the issue;
- warning or error messages;
- what you expected to happen instead.

For a short guided walkthrough, see [DEMO.md](DEMO.md).

## Current Status

`mlmr` is in public beta. The current version is suitable for demonstration,
teaching, usability testing, and structured feedback. Users should independently
verify model specification, convergence, diagnostics, and interpretation before
using results in production research.

## Roadmap

Near-term priorities:

- publish the `pkgdown` documentation site;
- add screenshots and a richer public demo guide;
- expand tests for formulas, centering, equations, tables, and exports;
- harden uploaded-data validation;
- prepare CRAN release materials;
- broaden crossed, longitudinal, GLMM, and multiple-membership workflows.

## License

MIT License.
