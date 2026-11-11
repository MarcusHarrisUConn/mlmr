# mlmr Software Article Readiness

This checklist tracks work needed for a peer-reviewed software article after
the first CRAN submission.

## Venue Fit

- R Journal: strongest near-term fit after CRAN acceptance.
- JOSS: good fit after more public development history, beta feedback, and
  user evidence.
- Journal of Statistical Software: possible later if the paper becomes a deeper
  tutorial or methodological software article.

## Required Before Submission

- [ ] CRAN release accepted.
- [ ] Tagged GitHub release matching the CRAN version.
- [ ] Zenodo archive and DOI for the release.
- [ ] Public documentation site updated for the release.
- [ ] Screenshots added to the article or supplementary materials.
- [ ] Screenshot checklist completed in `paper/SCREENSHOT_CHECKLIST.md`.
- [ ] Validation report generated from the release version.
- [ ] Benchmark validation expanded to stored fixed effects, standard errors,
      variance components, ICCs, diagnostics, and exported-code checks.
- [ ] Case study rendered from source code.
- [ ] `paper/paper.md` expanded to the target journal format.
- [ ] Bibliography includes R, lme4, Shiny, ggplot2, and relevant multilevel
      modeling references.
- [ ] Conflict of interest, funding, data availability, and AI-use statements
      drafted if the target venue requires them.

## Suggested Article Figures

- GUI workflow overview.
- Model builder with centering and random effects.
- Results equations tab.
- Publication bundle or validation report export.
- Diagram showing GUI decisions flowing into reproducible R code and manuscript
  artifacts.

## Suggested Case Study

Use the built-in HSB-style example to demonstrate:

1. selecting grouping factors;
2. declaring Level 1 and Level 2 predictors;
3. choosing cluster-mean and grand-mean centering;
4. fitting a random-slope model;
5. reviewing variance components and ICCs;
6. inspecting equations and diagnostics;
7. exporting reproducible code and publication materials.
