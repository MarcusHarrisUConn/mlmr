# mlmr

<!-- badges: start -->
[![R CMD check](https://github.com/MarcusHarrisUConn/mlmr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MarcusHarrisUConn/mlmr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`mlmr` is an open-source R package and Shiny application for specifying,
fitting, understanding, and reporting mixed-effects and multilevel models. It is
designed for researchers, instructors, graduate students, and applied analysts
who want an **HLM 8-style** level-by-level workflow without giving up
transparent, reproducible `lme4` R code.

The app and package guide users from data import, explicit clustering design
selection, inline centering decisions, random-slope toggles, and cross-level
slope moderators to live pre-fit symbolic equations, APA-style model-based and
cluster-robust sandwich SE tables, variance-component likelihood-ratio tests,
Level-1 variance homogeneity ($H$) tests, Nakagawa $R^2$, empirical Bayes
intercept and slope reliabilities ($\hat{\lambda}$), population-average HGLM
odds ratios, diagnostics, and reproducible `.zip` publication bundles.

## Why Use mlmr?

Many researchers learn multilevel modeling through level-based graphical
software (such as HLM 8), then need reproducible R syntax for manuscripts,
theses, teaching, and open science. `mlmr` bridges both worlds:

- **Immediate Home-Page Clarity**: **Data > Overview** highlights the active
  **Dependent Variable (Outcome $Y$)**, **Cluster Hierarchy**, and **Active
  Independent Variables** alongside live symbolic Level-1/Level-2/Level-3
  equations and an auto-classified **Dataset Variable Guide**.
- **Guided 4-Step HLM 8 Equation Builder**: Assign Level-1 and higher-level
  predictors with inline centering (`CWC`, `GMC`, uncentered), one-click
  random-slope (`+ u`) checkboxes, and dedicated **Cross-Level Slope Moderator**
  selectors—watching the hierarchical equations update live before fitting.
- **Global `Advanced / HGLM` Switch**: Toggle non-Gaussian outcomes (Binomial
  Logit/Probit, Poisson, Negative Binomial, Gamma) directly from the top
  navigation bar with unit-specific and population-average reporting.
- **Full HLM 8 Inferential Companion Suite**: Pairs `lme4` / `lmerTest` /
  `pbkrtest` estimates with Huber-White cluster-robust sandwich SEs (`CR1` /
  `CR0`), random-intercept and random-slope reliabilities, variance-component
  LRT $\chi^2$ tests, and Bartlett's $H$ test of Level-1 variance homogeneity.
- **Complete Reproducibility**: Every GUI run pairs results with standalone `.R`
  scripts, Quarto `.qmd` reports, raw LaTeX (`amsmath` + `graphicx`), and `.zip`
  archive bundles.

## What the App Looks Like

- **Data**: choose built-in HSB educational or longitudinal growth datasets, or
  upload CSV, TSV/TXT, Excel, SPSS, SAS, or Stata files; declare the Dependent
  Variable and clustering design (*Two-Level Nested*, *Three-Level Nested*,
  *Longitudinal*, or *Cross-Classified*); and inspect the **Active Multilevel
  Model Blueprint** on **Overview**.
- **Model**: step through Outcome, Level-1 Predictors (centering + random
  slopes), Higher-Level Predictors & Cross-Level Slope Moderators, and
  $\mathbf{T}$ covariance correlation controls alongside live symbolic HLM
  equations.
- **Estimate**: choose REML/ML, Gaussian degrees of freedom (`none`,
  `satterthwaite`, or `kenward-roger`), verify pre-fit **Model Readiness**
  checks, and fit or refit.
- **Results**: review dashboard summaries, Model-Based and Cluster-Robust APA
  tables, variance LRTs, intercept & slope reliabilities, Level-1 homogeneity
  ($H$), population-average ORs (for binary logit HGLMs), fitted equations,
  complete-case diagnostics, and likelihood-ratio model comparisons.
- **Report & Code**: copy or download Quarto `.qmd` reports, standalone `.R`
  scripts, `.tex` equations/tables, Word/HTML APA tables, and `.zip`
  reproducibility archives.

Screenshots and documentation are available at:

<https://marcusharrisphd.com/mlmr/>

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

# Preview symbolic HLM equations before fitting
mlmr::mlm_spec_equations(spec)

# Fit and compute HLM 8 + APA outputs
fit <- mlmr::mlm_fit(spec, df_method = "satterthwaite")
mlmr::mlm_apa_tables(fit)
mlmr::mlm_robust_se(fit)
mlmr::mlm_variance_tests(fit)
mlmr::mlm_homogeneity_test(fit)
mlmr::mlm_r2(fit)
mlmr::mlm_reliability(fit)
mlmr::mlm_latex_equations(fit)
mlmr::mlm_write_publication_bundle(fit, "mlmr_bundle.zip")
```

To inspect scope, the 24-workflow HLM 8 parity matrix, and deterministic
validation benchmarks:

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
