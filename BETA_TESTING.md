# mlmr Beta Testing Guide

This guide is for early testers of `mlmr`. The goal is to evaluate whether the
app helps users specify, fit, understand, and report mixed-effects and
multilevel models in a clear and reproducible way.

## Install the Beta

```r
install.packages("pak")
pak::pak("MarcusHarrisUConn/mlmr")
mlmr::run_mlmr()
```

If you prefer `remotes`:

```r
install.packages("remotes")
remotes::install_github("MarcusHarrisUConn/mlmr")
mlmr::run_mlmr()
```

## What To Test First

### 1. Built-in example workflow

1. Launch the app with `mlmr::run_mlmr()`.
2. Keep the built-in HSB-style example data.
3. Review the **Data** tab and confirm that grouping IDs, variable roles, and
   missingness summaries are understandable.
4. Open the **Model** tab and inspect outcome, declared predictor levels,
   centering, interactions, and random effects.
5. Fit the example model.
6. Review Results, Tables, Equations, Diagnostics, and Report & Code.

### 2. Two-level model workflow

Try a model with one grouping factor, such as students within schools, patients
within clinics, repeated observations within people, or employees within teams.

Please check:

- whether the grouping factor is easy to declare;
- whether Level 1 and Level 2 predictors are clear;
- whether centering choices are understandable;
- whether the random-intercept and random-slope choices match your intended
  model;
- whether the exported R code reproduces the fitted model.

### 3. Three-level model workflow

Try a model with two higher-level grouping factors, such as students within
schools within districts.

Please check:

- whether the hierarchy is easy to represent;
- whether the formula contains the random-effect blocks you expected;
- whether ICCs and variance components are understandable;
- whether the equations and Tau matrix help explain the model.

### 4. Reporting workflow

After fitting a model, test the reporting outputs:

- APA fixed-effects table;
- dummy-coding table;
- variance components table;
- ICC table;
- level-by-level equations;
- combined equation;
- Tau variance-covariance matrix;
- reproducible R code;
- raw LaTeX;
- Quarto-ready report;
- software/package citation statement.

Please paste the raw LaTeX into your usual editor, such as Overleaf or Quarto,
and report whether the tables or equations need adjustment.

## Current Production Scope

The strongest current production path is Gaussian two-level and three-level
nested mixed-effects models with random intercepts, random slopes, centering,
interactions, APA tables, equations, and reproducible exports.

Advanced/experimental workflows include GLMMs and crossed random-effect
structures. Weighted multiple-membership models are not yet in the production
scope.

You can inspect the current scope from R:

```r
mlmr::mlm_supported_models()
```

## What Feedback Is Most Helpful

Please open an issue at:

<https://github.com/MarcusHarrisUConn/mlmr/issues>

Helpful feedback includes:

- the model structure you tried;
- whether you used the example data or uploaded data;
- the file type you uploaded;
- screenshots of confusing screens;
- generated R code or LaTeX that did not work;
- warning or error messages;
- what you expected the app to do instead.

## What To Verify Before Reporting Results

`mlmr` helps with specification and reporting, but users should still verify:

- the outcome distribution and link function;
- grouping IDs and nesting/crossing structure;
- centering choices;
- contrast/reference categories;
- convergence warnings;
- singular fits;
- residual plots and diagnostics;
- whether random slopes are supported by the data;
- whether exported manuscript language matches the research question.

## Beta Testing Goal

The beta is successful if testers can:

1. fit the example model without confusion;
2. fit at least one model with their own data;
3. understand the model formula and equations;
4. export tables, equations, and reproducible code;
5. identify any places where the app makes the workflow unclear, too crowded, or
   statistically ambiguous.
