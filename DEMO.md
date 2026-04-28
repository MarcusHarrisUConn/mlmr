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

1. Launch the app.
2. On **Data**, keep the built-in example data selected.
3. Skim **Data > Structure** and **Data > Roles** to see the grouping factors
   and variable roles.
4. Open **Model** and review the preset outcome, predictors, centering choices,
   interactions, and random effects.
5. Click **Fit Example Model** from **Data > Overview**, or open **Estimate**
   and click **Fit Example Model**.
6. Open **Results > Tables** and inspect the APA-style tables.
7. Open **Results > Equations** and inspect the level-by-level equations,
   combined equation, and Tau matrix.
8. Open **Results > Diagnostics** and review model diagnostics.
9. Open **Report & Code** and inspect the reproducible R code and LaTeX export.

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
