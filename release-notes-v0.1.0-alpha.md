# mlmr v0.1.0-alpha

This is the first public alpha release of `mlmr`, an open-source R package and
Shiny app for guided mixed-effects and multilevel modeling with `lme4`.

## What This Release Is For

This release is intended for demonstration, teaching, usability testing, and
structured feedback. It is ready for early users to install from GitHub, try the
built-in example, upload their own data, and evaluate the app workflow.

The strongest current production path is Gaussian two-level and three-level
nested mixed-effects models with random intercepts, random slopes, centering,
interactions, APA-style tables, equations, Tau matrix displays, diagnostics, and
reproducible exports.

## Install

```r
install.packages("pak")
pak::pak("MarcusHarrisUConn/mlmr")
mlmr::run_mlmr()
```

## Highlights

- Guided Shiny app for model building and reporting
- Built-in HSB-style example data with preset model choices
- User-declared Level 1, Level 2, and Level 3 predictor structure
- Grand-mean and cluster-mean centering
- Random intercepts and random slopes
- Correlated and independent random-effect structures
- Fixed interactions and cross-level interaction-style terms
- APA-style fixed effects, variance components, ICC, dummy-coding, and software
  tables
- Level-by-level equations, combined equations, and Tau variance-covariance
  matrix displays
- Reproducible R code, raw LaTeX, Quarto-ready reports, and software citation
  support
- Import support for CSV, TSV/TXT, Excel, SPSS, SAS, and Stata files when
  optional reader packages are installed

## Advanced or Experimental

- Binomial, Poisson, negative binomial, and Gamma GLMM workflows
- Crossed random-effect structures
- Longitudinal/repeated-measures structures represented through grouping
  variables

Weighted multiple-membership models are not yet in the production scope.

## Documentation

- Package site: https://marcusharrisphd.com/mlmr/
- Beta testing guide: https://marcusharrisphd.com/mlmr/BETA_TESTING.html
- Supported models and scope:
  https://marcusharrisphd.com/mlmr/articles/supported-models.html

## Feedback

Please use the GitHub issue tracker for alpha feedback:

https://github.com/MarcusHarrisUConn/mlmr/issues

Helpful feedback includes the model structure you tried, whether you used the
example or uploaded data, screenshots of confusing output, generated R code or
LaTeX that did not work, and warning/error messages.
