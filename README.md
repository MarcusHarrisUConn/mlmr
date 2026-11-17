# mlmr

<!-- badges: start -->
[![R CMD check](https://github.com/MarcusHarrisUConn/mlmr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MarcusHarrisUConn/mlmr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`mlmr` is an open-source R package and Shiny app for fitting, understanding,
and reporting mixed-effects and multilevel models. It is designed for
researchers, instructors, graduate students, and applied analysts who want a
guided HLM-style model-building workflow without giving up transparent `lme4` R
code.

The app and package guide users from data import, grouping structure, centering
decisions, fixed effects, per-level random effects, and interactions to
APA-style tables, Nakagawa $R^2$, random-intercept reliability, level-by-level
equations, combined equations, labeled Tau variance-covariance displays,
diagnostics, and reproducible `.zip` publication bundles.

Gaussian linear mixed models (nested, three-level, and longitudinal growth) are
the primary production workflow, with optional Satterthwaite and Kenward-Roger
denominator degrees of freedom via `lmerTest` and `pbkrtest`. GLMM workflows
(binomial, Poisson, negative binomial, and Gamma) are available in Advanced Mode
with explicit link-scale, Wald-inference, and odds-ratio / incidence-rate-ratio
reporting.

## Why Use mlmr?

Many researchers learn multilevel modeling through level-based graphical
software, then need reproducible R syntax for manuscripts, theses, teaching, and
open science. `mlmr` bridges those worlds:

- model setup feels familiar to users who think in Level 1, Level 2, and Level 3
  terms;
- fitted models remain ordinary `lme4` (or `lmerTest`) models;
- centering, contrast coding, interactions, degrees of freedom, and
  random-effects structures are made explicit;
- every GUI run pairs results with reproducible R scripts, Quarto reports, raw
  LaTeX (`amsmath` + `graphicx`), and `.zip` archive bundles.

## What the App Looks Like

- **Data**: choose built-in HSB educational or longitudinal growth datasets, or
  upload CSV, TSV/TXT, Excel, SPSS, SAS, or Stata files (including labelled
  categorical variables).
- **Model**: declare outcome, grouping factors, predictor levels, centering,
  interactions, and per-grouping-factor random intercepts, slopes, and Tau
  correlations with a live covariance matrix preview.
- **Estimate**: choose REML/ML, Gaussian degrees of freedom (`none`,
  `satterthwaite`, or `kenward-roger`), verify pre-fit **Model Readiness**
  checks, and fit or refit.
- **Results**: review dashboard summaries, APA tables, $R^2$ and reliability,
  level-by-level and combined equations, labeled Tau matrices, complete-case
  diagnostics, and likelihood-ratio model comparisons.
- **Report & Code**: copy or download Quarto `.qmd` reports, standalone `.R`
  scripts, `.tex` equations/tables, Word/HTML APA tables, and `.zip`
  reproducibility archives.

Screenshots and documentation are available at:

<https://marcusharrisphd.com/mlmr/>

![mlmr model builder](https://marcusharrisphd.com/mlmr/screenshots/mlmr-model-builder.png)

![mlmr results dashboard](https://marcusharrisphd.com/mlmr/screenshots/mlmr-results-dashboard.png)

## Installation

Install `mlmr` from GitHub with `pak` or `remotes`:

```r
install.packages("pak")
pak::pak("MarcusHarrisUConn/mlmr")
```

```r
install.packages("remotes")
remotes::install_github("MarcusHarrisUConn/mlmr")
```

For small-sample Satterthwaite or Kenward-Roger degrees of freedom in Gaussian
models, install the optional inference packages:

```r
install.packages(c("lmerTest", "pbkrtest"))
```

## Launch the App

```r
mlmr::run_mlmr()
```

During local development from a repository checkout:

```r
shiny::runApp(".")
```

## Docker

```bash
docker build -t mlmr:local .
docker run --rm -p 3838:3838 mlmr:local
```

Or with Docker Compose:

```bash
docker compose up --build
```

See [DOCKER.md](https://github.com/MarcusHarrisUConn/mlmr/blob/main/DOCKER.md)
for GitHub Container Registry details.

## Use the Backend Directly in R

```r
dat <- mlmr::example_hsb()

spec <- mlmr::mlm_spec(
  outcome = "mathscore",
  fixed = list(
    ses = list(center = "CWC"),
    meanses = list(center = "GMC"),
    sector = list(center = "none")
  ),
  grouping = list(schoolid = "schoolid"),
  random = list(
    schoolid = list(intercept = TRUE, slopes = "ses", correlation = TRUE)
  ),
  interactions = list(c("ses", "sector")),
  data = dat
)

fit <- mlmr::mlm_fit(spec, df_method = "satterthwaite")
mlmr::mlm_apa_tables(fit)
mlmr::mlm_r2(fit)
mlmr::mlm_reliability(fit)
mlmr::mlm_latex_equations(fit)
mlmr::mlm_write_publication_bundle(fit, "mlmr_bundle.zip")
```

For repeated-measures / growth-curve workflows:

```r
dat_long <- mlmr::example_longitudinal()
```

To inspect scope, HLM 8 parity, and deterministic validation benchmarks:

```r
mlmr::mlm_supported_models()
mlmr::mlm_hlm8_parity()
mlmr::mlm_validation_benchmarks(run = TRUE)
mlmr::mlm_validation_report()
```

## Documentation

```r
vignette("getting-started", package = "mlmr")
vignette("supported-models", package = "mlmr")
vignette("equations-and-reporting", package = "mlmr")
vignette("hlm-parity-and-validation", package = "mlmr")
```

Public documentation site: <https://marcusharrisphd.com/mlmr/>

## License

MIT License.
