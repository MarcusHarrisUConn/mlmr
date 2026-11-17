# Contributing to mlmr

Thank you for helping test and improve `mlmr`.

## Useful Feedback

Please open a GitHub issue if you notice:

- a model specification that should fit but fails;
- confusing model-building language;
- incorrect or unclear equations or Tau matrix labels;
- APA table formatting problems;
- uploaded data files that the app handles poorly;
- diagnostics that are missing or hard to interpret;
- places where exported R code, Quarto reports, or LaTeX do not match the app.

## Development Workflow

All modeling, reporting, and Shiny UI/server code live in modular single-source
files under `R/`, with the stylesheet in `inst/app/www/style.css` (no duplicate
copy syncing is needed).

Before opening a pull request, run:

```r
source("dev/check-publication-readiness.R")
devtools::test()
```

And run a full CRAN-style package check:

```sh
R CMD build .
R CMD check --no-manual --as-cran mlmr_0.1.0.tar.gz
```

Tests live in `tests/testthat/` and cover model specification, centering,
formula generation, readiness guardrails, equations, APA tables, $R^2$ and
reliability, denominator degrees of freedom, complete-case diagnostics, LRT
model comparisons, `.zip` publication bundles, reproducible-code round trips,
and Shiny server workflows.
