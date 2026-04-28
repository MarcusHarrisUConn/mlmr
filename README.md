# mlmr

`mlmr` is an open-source Shiny app and R toolkit for fitting, understanding,
and reporting mixed-effects and multilevel models in R.

The package is designed for researchers who want a guided graphical workflow
while preserving transparent, reproducible R code. Under the hood, `mlmr` uses
`lme4` and helps users build models, choose centering strategies, inspect
random-effects structures, generate equations, create APA-style tables, and
export reproducible code and LaTeX.

## Current Features

- Two- and three-level mixed-effects model workflows
- Random intercepts and random slopes
- Correlated and independent random-effects structures
- Grand-mean and cluster-mean centering controls
- Interaction and cross-level interaction support
- APA-style fixed-effects, variance components, ICC, and dummy-coding tables
- Level-by-level and combined model equations
- Tau variance-covariance matrix display
- Diagnostics and model comparison tools
- Built-in HSB-style example data with preset model choices
- Shiny interface plus reproducible R code, Quarto, and LaTeX export

## Public Beta Feedback

`mlmr` is ready for demo testing and early feedback. If you try the app, please
open a GitHub issue with comments about the model-building workflow, equations,
APA tables, diagnostics, generated code, or uploaded-data experience:

<https://github.com/MarcusHarrisUConn/mlmr/issues>

The most helpful feedback includes the model structure, whether you used the
built-in example or uploaded data, screenshots of confusing output, and any
generated code or error messages.

## Why mlmr?

Mixed-effects and multilevel models are powerful, but many researchers learn
them through software workflows that hide the model formula, centering choices,
equations, variance components, and reporting code. `mlmr` aims to make those
pieces visible.

The goal is not only to fit a model, but to help users understand what was fit,
how the random-effects structure was specified, how predictors were centered,
and how to reproduce the analysis in a manuscript-ready workflow.

## Installation

The package is in early development. After the GitHub repository is public, you
can install the development version with:

```r
install.packages("remotes")
remotes::install_github("MarcusHarrisUConn/mlmr")
```

## Run the App

After installation:

```r
mlmr::run_mlmr()
```

During local development, run the app directly from the project folder:

```r
shiny::runApp(".")
```

## Project Structure

- `app.R` - Shiny UI and server for local development
- `R/mlm_core.R` - model specification, formula generation, fitting,
  diagnostics, equations, tables, and code generation
- `R/run_mlmr.R` - package launch helper
- `inst/app/` - installed-package copy of the Shiny app
- `www/style.css` - app styling
- `man/` - package documentation

## Roadmap

- Strengthen unit tests for formula generation, centering, equations, tables,
  and exports
- Add package vignettes for two-level models, three-level models, centering,
  equations, and reporting
- Build a `pkgdown` documentation site
- Expand support for crossed random effects, longitudinal models, GLMMs, and
  multiple-membership structures
- Prepare for a public beta release before CRAN submission

## License

MIT License.
