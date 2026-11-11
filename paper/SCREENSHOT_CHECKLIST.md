# mlmr Article Screenshot Checklist

Use these screenshots for the software paper, package website, and reviewer
materials. Capture images from the installed or release-candidate version of
the app, not from an experimental working tree.

## Required Screenshots

- [ ] Data overview with built-in example selected.
  - Purpose: show the low-friction starting point for teaching and demos.
  - Suggested filename: `mlmr-data-overview.png`
- [ ] Model builder with outcome, predictor levels, centering, interactions,
      and random effects visible.
  - Purpose: show the HLM-style model specification workflow.
  - Suggested filename: `mlmr-model-builder.png`
- [ ] Estimate tab with model-readiness checks.
  - Purpose: show guardrails before fitting custom models.
  - Suggested filename: `mlmr-model-readiness.png`
- [ ] Results dashboard after fitting the built-in example.
  - Purpose: show immediate model summary and guidance.
  - Suggested filename: `mlmr-results-dashboard.png`
- [ ] Results tables tab with fixed effects and variance components visible.
  - Purpose: show manuscript-oriented output.
  - Suggested filename: `mlmr-results-tables.png`
- [ ] Equations tab with level-by-level equations and Tau structure.
  - Purpose: show the teaching value beyond a coefficient table.
  - Suggested filename: `mlmr-results-equations.png`
- [ ] Diagnostics tab with residual and fitted-value plots.
  - Purpose: show model-checking workflow.
  - Suggested filename: `mlmr-diagnostics.png`
- [ ] Report & Code tab showing the publication checklist and methods text.
  - Purpose: show reproducibility and manuscript support.
  - Suggested filename: `mlmr-publication-bundle.png`
- [ ] About tab with HLM-style feature parity and validation benchmarks.
  - Purpose: show software scope and reviewer-facing validation evidence.
  - Suggested filename: `mlmr-validation-scope.png`

## Capture Notes

- Use a desktop browser width around 1400 pixels so tables and equations are
  readable.
- Fit the built-in example model before capturing Results, Diagnostics, and
  Report & Code screenshots.
- Avoid screenshots that show local file paths, browser bookmarks, private
  tabs, or desktop notifications.
- Prefer light mode for article figures unless the target journal specifies
  otherwise.
- Store final article screenshots under `paper/figures/` and website
  screenshots under `pkgdown/assets/screenshots/`.

## Optional Figure

- [ ] Workflow diagram showing app decisions flowing into reproducible R code,
      validation reports, and manuscript artifacts.
  - Purpose: orient software-paper readers before the case study.
  - Suggested filename: `mlmr-workflow-diagram.png`
