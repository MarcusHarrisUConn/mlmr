# Publication Readiness Plan

This checklist tracks the next ten items that most directly move `mlmr` from a
strong public beta toward a CRAN-ready, classroom-ready, and manuscript-ready
release.

## Next 10 Items

1. **CRAN metadata and checks**
   - Keep `DESCRIPTION`, `cran-comments.md`, `CRAN_SUBMISSION.md`, and
     `inst/CITATION` current.
   - Evidence: local `R CMD check --as-cran` and GitHub Actions checks pass.

2. **Installed-package launch reliability**
   - Verify `mlmr::run_mlmr()` launches from an installed package, not only from
     the development folder.
   - Evidence: installed-package smoke test responds over HTTP.

3. **Manuscript output regression tests**
   - Protect APA tables, raw LaTeX, equations, Tau matrices, software reporting,
     and Quarto report text with backend tests.
   - Evidence: `tests/backend-tests.R` covers these output surfaces.

4. **Model-specification validation**
   - Prefer clear errors for missing variables, unsupported distributions, and
     invalid example-data arguments.
   - Evidence: constructor and example-data tests cover invalid inputs.

5. **App/backend consistency**
   - Keep reusable backend code in `R/` synchronized with the installed Shiny app
     copy in `inst/app/R/`.
   - Evidence: tests compare the shipped app backend to the package backend.

6. **Beta feedback workflow**
   - Make it easy for testers to report model structure, exported-code checks,
     and confusing UI sections.
   - Evidence: GitHub issue templates and `BETA_TESTING.md`.

7. **Supported-models scope**
   - Clearly distinguish supported, experimental, planned, and out-of-scope
     model features.
   - Evidence: `mlm_supported_models()` and the supported-models vignette.

8. **Deployment/demo reliability**
   - Maintain Docker and local launch paths for workshops, demo sessions, and
     reproducible testing.
   - Evidence: Dockerfile, Compose file, Docker Actions workflow, and
     `DOCKER.md`.

9. **Documentation polish**
   - Keep README, vignettes, pkgdown, screenshots, demo guide, and PDF manual
     aligned with the current app behavior.
   - Evidence: pkgdown deploy passes and docs contain no local machine paths.

10. **Accessibility and novice-user UX**
    - Continue checking color contrast, mobile navigation, loading states,
      redundant panels, and explanatory labels.
    - Evidence: manual click-through plus beta tester feedback.

## Current Submission Gate

Do not submit to CRAN until all of the following are true:

- `R CMD check --as-cran` has no errors or warnings and only expected notes.
- GitHub Actions passes on Windows, macOS, Ubuntu release, and Ubuntu devel.
- The app launches from an installed package.
- The built source tarball contains no development artifacts or local paths.
- A human click-through of the built-in example model succeeds.
- The public documentation site accurately states the current production scope.

## Local Rehearsal Command

Run the maintainer readiness script before any beta tag or CRAN upload:

```r
source("dev/check-publication-readiness.R")
```

This script checks synchronized app/backend copies, public documentation for
local machine paths, expected release files, and the latest source tarball for
excluded maintainer artifacts.
