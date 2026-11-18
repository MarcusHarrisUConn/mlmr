# mlmr Public Beta Demo Guide

Thank you for trying `mlmr`.

`mlmr` is an open-source Shiny app and R toolkit for fitting, understanding,
and reporting mixed-effects and multilevel models in R. The current version is
an early public beta intended for demonstration, teaching, and structured
feedback.

## Install

```r
install.packages("remotes")
remotes::install_github("MarcusHarrisUConn/mlmr")
```

## Launch

```r
mlmr::run_mlmr()
```

## Five-Minute Demo

1. Launch the app (`mlmr::run_mlmr()`).
2. On **Data > Overview**, review the **Active Multilevel Model Blueprint**
   showing the Dependent Variable (`mathscore`), Multilevel Clustering Structure
   (`schoolid`), Independent Variables, and live pre-fit symbolic equations.
3. Skim **Data > Structure** and **Data > Roles** to inspect cluster sizes and
   empirical variable variation.
4. Open **Model** to customize predictors, inline centering (`CWC` / `GMC`),
   random slopes (`+ u`), and cross-level slope moderators while watching the
   symbolic equations update live.
5. Click **Fit Current Model** from **Data > Overview** (or **Estimate**).
6. Open **Results > Tables** to compare Model-Based and Cluster-Robust (`CR1`)
   APA tables, multiparameter Wald tests, variance LRTs, ICCs, $R^2$, and
   empirical Bayes reliabilities.
7. Open **Results > Equations**, **Results > Interpretation** (interactive
   simple-slopes and forest plots), **Results > Diagnostics**, and **Results >
   Compare** (one-click 3-step `Null -> Level 1 -> Full` side-by-side APA
   comparison table).
8. Open **Report & Code** to download the Quarto `.qmd` report, standalone `.R`
   script, raw LaTeX, or `.zip` publication bundle.

## Visual Walkthrough

The screenshots below show the built-in example workflow. They are intended for
orientation and for beta testers who want to know what a successful fit should
look like.

### Data Overview

![Data overview](https://marcusharrisphd.com/mlmr/screenshots/mlmr-data-overview.png)

### Model Builder

![Model builder](https://marcusharrisphd.com/mlmr/screenshots/mlmr-model-builder.png)

### Results Dashboard

![Results dashboard](https://marcusharrisphd.com/mlmr/screenshots/mlmr-results-dashboard.png)

### APA Tables

![APA tables](https://marcusharrisphd.com/mlmr/screenshots/mlmr-results-tables.png)

### Equations

![Equations](https://marcusharrisphd.com/mlmr/screenshots/mlmr-results-equations.png)

### Report and Code

![Report and code](https://marcusharrisphd.com/mlmr/screenshots/mlmr-report-code.png)

## What to Evaluate

Please pay attention to:

- whether the model-building workflow is understandable;
- whether centering choices are clear;
- whether random intercepts, random slopes, and Tau matrices are explained well;
- whether equations look correct and manuscript-ready;
- whether APA tables are useful;
- whether the exported R code reproduces what the app did;
- whether uploaded CSV, TSV/TXT, Excel, SPSS, SAS, or Stata files are easy to
  use;
- whether the **Model Readiness** checklist catches problems before fitting;
- whether diagnostics and warnings are interpretable.

## Feedback

Please submit feedback through GitHub Issues:

<https://github.com/MarcusHarrisUConn/mlmr/issues>

Use the **Demo feedback** template when possible.

Helpful feedback includes:

- the model you tried to fit;
- whether you used example or uploaded data;
- screenshots of confusing output;
- generated R code if reproducibility was the issue;
- warning or error messages;
- what you expected to happen instead.

## Current Limitations

The app is still in beta. Some advanced structures are scaffolded but not fully
polished. Users should independently verify model specification, convergence,
diagnostics, and interpretation before using results in production research.
