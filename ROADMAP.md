# mlmr Roadmap

`mlmr` has completed its core public-beta stabilization, single-source package
refactoring, and CRAN/software-article validation milestones.

## Completed Milestones (v0.1.0)

- [x] Unified single-source modular `R/` package and Shiny architecture (zero
      duplicated `inst/app/` backend files).
- [x] Pre-fit uploaded-data validation for `haven_labelled` SPSS/SAS/Stata
      imports, omitted interaction main effects, constant/high-cardinality
      predictors, continuous numeric grouping IDs, and multi-level variation.
- [x] Per-grouping-factor random intercepts, random slopes, and Tau covariance
      preview.
- [x] Optional Satterthwaite and Kenward-Roger denominator degrees of freedom
      via `lmerTest` and `pbkrtest`.
- [x] Nakagawa marginal and conditional $R^2$ (`mlm_r2()`), empirical Bayes
      intercept & slope reliabilities (`mlm_reliability()`), cluster-robust
      sandwich SEs (`mlm_robust_se()`), multiparameter Wald tests
      (`mlm_wald_test()`), variance LRTs (`mlm_variance_tests()`), Level-1
      variance homogeneity (`mlm_homogeneity_test()`), and residual exports
      (`mlm_residual_files()`).
- [x] Longitudinal growth-curve dataset (`example_longitudinal()`) and workflow
      template.
- [x] Row/column-labeled Tau matrices, fixed-vs-random Coefficient Key roles,
      and wrapped multi-term LaTeX equations.
- [x] One-click `.zip` publication & reproducibility archive
      (`mlm_write_publication_bundle()`).
- [x] Gold-standard 8-model deterministic validation suite
      (`mlm_validation_benchmarks(run = TRUE)`) checking AICs, point estimates,
      SEs, variances, ICCs, and round-trip exported R script execution.

## Post-0.1.0 Enhancements (v0.2.0+)

- Simple-slopes probing and marginal-effects interaction plots.
- Parametric and cluster bootstrap confidence intervals for variance components,
  ICCs, and $R^2$.
- Cluster-level Cook's distance and leverage diagnostics.
- Side-by-side multi-model APA comparison tables (Model 1 / Model 2 / Model 3
  columns) and native `.docx` export.
- GUI-integrated Rubin's rules pooling for multiply imputed multilevel datasets.
