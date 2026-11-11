---
title: "mlmr: A Free Graphical Workflow for Teaching and Reporting Multilevel Models in R"
tags:
  - R
  - Shiny
  - multilevel modeling
  - mixed-effects models
  - reproducible research
authors:
  - name: Marcus Harris
    affiliation: 1
affiliations:
  - name: University of Connecticut
    index: 1
date: 2026-07-01
bibliography: paper.bib
---

# Summary

`mlmr` is an open-source R package and Shiny application for fitting,
understanding, and reporting multilevel and mixed-effects models. The software
is designed as a free, reproducible alternative to expensive HLM-style
graphical workflows: users can build models through a guided interface while
retaining transparent `lme4` syntax, equations, diagnostics, APA-style tables,
and manuscript-ready exports.

# Statement of Need

Multilevel and mixed-effects models are now routine in education, psychology,
health, organizational research, and other fields where observations are
clustered or repeatedly measured. Many students and applied researchers first
learn these models through level-based graphical workflows, but publication and
open-science workflows increasingly require reproducible code, transparent
software versions, and portable analysis records.

`mlmr` bridges these needs. The graphical interface lets users reason in terms
of outcomes, grouping factors, predictor levels, centering decisions, random
effects, and interactions. The backend keeps the model as ordinary R code built
on `lme4` [@lme4], so users can inspect, rerun, modify, and publish the full
analysis.

The package is not intended to replace statistical judgment or specialist
software for every possible mixed-model design. Instead, it provides a free,
teachable, and reproducible workflow for common HLM-style modeling tasks.

# Design

`mlmr` has two connected surfaces. First, the Shiny application [@shiny] guides
users through data review, model specification, estimation, diagnostics,
results, equations, and exports. Second, the package exposes backend functions
for users who want to script the same workflow directly in R [@R].

The design principle is that every GUI decision should leave a reproducible
trace. Centering choices, random-effect structures, model formulas, software
versions, APA-style tables, equations, diagnostics, and report text can all be
exported or recreated from package functions.

# Functionality

The package currently supports two- and three-level nested workflows,
random-intercept and random-slope models, centering decisions, interactions,
model diagnostics, equation displays, APA tables, software citations, and
Quarto/R/LaTeX exports. Gaussian linear mixed models are the primary production
workflow. GLMM presets are available as advanced workflows with explicit
link-scale, Wald-inference, and family-specific reporting cautions. The function
`mlm_hlm8_parity()` tracks HLM-style feature parity, while
`mlm_validation_benchmarks()` and `mlm_validation_report()` provide
deterministic validation templates and fitted-output summaries for regression
testing.

The main user workflow is:

1. review data, grouping factors, missingness, and variable roles;
2. select model templates or specify a custom model;
3. choose centering, interactions, and random effects;
4. fit or refit the model;
5. inspect equations, tables, diagnostics, and model comparisons;
6. export reproducible R code, Quarto reports, LaTeX equations, APA tables,
   software statements, and publication-readiness materials.

# Example Analysis

The package includes a built-in HSB-style educational dataset generator. The
example data include student-level predictors, school-level predictors,
categorical variables, interactions, and multiple grouping identifiers. This
lets instructors and users demonstrate random-intercept models, random-slope
models, cross-level interaction-style terms, and three-level structures without
requiring external files.

A typical analysis begins with:

```r
dat <- mlmr::example_hsb()
spec <- mlmr::mlm_spec(
  outcome = "mathscore",
  fixed = list(
    ses = list(center = "CWC"),
    meanses = list(center = "GMC")
  ),
  grouping = list(schoolid = "schoolid"),
  random = list(
    schoolid = list(intercept = TRUE, slopes = "ses", correlation = TRUE)
  ),
  data = dat
)
fit <- mlmr::mlm_fit(spec)
mlmr::mlm_apa_tables(fit)
mlmr::mlm_latex_equations(fit)
```

The same model can be constructed in the GUI and exported as reproducible code.

# Validation

The package includes backend tests for formula generation, centering,
estimation, tables, equations, diagnostics, supported-model scope,
publication bundles, and reproducible-code exports. Continuous integration
runs `R CMD check` across major platforms.

The validation helper `mlm_validation_report()` fits deterministic benchmark
models and returns benchmark status, fixed effects, variance components,
intraclass correlations, and diagnostics. These benchmarks currently cover
null, random-intercept, random-slope, cross-level interaction, and three-level
models. This report is available from both R and the Shiny app.

Before journal submission, the benchmark suite should be expanded to include
stored reference estimates, standard errors, variance components, ICCs,
diagnostic expectations, and exported-code checks across supported model
families.

# Documentation and Availability

The package includes vignettes for getting started, supported models, equations
and reporting, and HLM-style parity and validation. The CRAN-style package
check currently passes with no errors or warnings in local rehearsal. The
source package builds vignettes into `inst/doc`, and the reference manual is
generated from the package help pages.

The source code is available at <https://github.com/MarcusHarrisUConn/mlmr>.
Documentation is available at <https://marcusharrisphd.com/mlmr/>.

# Acknowledgements

`mlmr` builds on the R ecosystem, including `lme4` for model fitting, Shiny for
the graphical interface, and `ggplot2` [@ggplot2] for diagnostic graphics.

# References
