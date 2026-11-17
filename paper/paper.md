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
date: 2026-11-17
bibliography: paper.bib
---

# Summary

`mlmr` is an open-source R package and Shiny application for specifying,
fitting, understanding, and reporting multilevel and mixed-effects models. The
software provides a free, reproducible alternative to proprietary point-and-click
multilevel workflows: users construct models through a guided level-based
interface while retaining transparent `lme4` [@lme4] and `lmerTest` [@lmerTest]
syntax, hierarchical equations, labeled Tau covariance matrices, diagnostics,
APA-style tables, effect sizes, and manuscript-ready `.zip` export bundles.

# Statement of Need

Multilevel and mixed-effects models are routine across education, psychology,
health sciences, and organizational research whenever observations are nested in
clusters or repeated over time [@raudenbush2002]. Students and applied
researchers frequently learn multilevel modeling through level-by-level
equations and point-and-click menus, whereas modern publication and open-science
standards demand executable R scripts, explicit centering documentation
[@enders2007], software version reporting, and portable replication archives.

`mlmr` bridges these two workflows. The Shiny application [@shiny] lets users
declare outcomes, grouping structures, Level 1–3 predictors, grand-mean and
cluster-mean centering, interactions, and per-level random effects without
memorizing formula syntax. Every graphical choice translates deterministically
into an exported `mlm_spec` object and standalone `lme4` script in R [@R].

# Architecture and Functionality

`mlmr` is organized as a single-source R package in which both the interactive
Shiny interface (`run_mlmr()`) and programmatic R helpers share the same
validated modeling and reporting engine:

- **Designs and estimation**: two- and three-level nested linear mixed models,
  longitudinal growth curves (`example_longitudinal()`), cross-classified random
  effects, and advanced generalized linear mixed models (binomial, Poisson,
  negative binomial, and Gamma) with optional Satterthwaite and Kenward-Roger
  denominator degrees of freedom [@lmerTest].
- **Data import and readiness guardrails**: CSV, TSV, Excel, SPSS, SAS, and
  Stata uploads (with automatic conversion of `haven` value labels), paired with
  pre-fit readiness checks for nesting hierarchy, random-slope within-cluster
  variation, omitted interaction main effects, constant or high-cardinality
  predictors, and binomial separation.
- **Equations and reporting**: hierarchical ($\beta/\gamma$ and
  $\pi/\beta/\gamma$) and mixed-model LaTeX/HTML equations, row/column-labeled
  Tau covariance matrices, APA 7th tables, marginal and conditional $R^2$
  [@nakagawa2017], empirical Bayes random-intercept reliability
  [@raudenbush2002], complete-case diagnostic graphics [@ggplot2], ML-refit
  likelihood-ratio comparisons, and one-click `.zip` publication bundles.

# Example Analysis

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
```

# Validation and Availability

Continuous integration runs `R CMD check --as-cran` across Windows, macOS, and
Linux (R-release and R-devel). In addition to unit and `shiny::testServer()`
regression suites, `mlm_validation_benchmarks(run = TRUE)` validates eight
deterministic reference specifications (null, random-intercept, random-slope,
cross-level interaction, three-level nested, crossed, longitudinal growth, and
binary logistic GLMM) against stored fixed-effect estimates, standard errors,
variance components, ICCs, and automated round-trip execution of exported R
scripts.

Source code is available under the MIT License at
<https://github.com/MarcusHarrisUConn/mlmr>, with documentation at
<https://marcusharrisphd.com/mlmr/>.

# References
