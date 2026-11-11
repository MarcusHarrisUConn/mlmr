# mlmr CRAN Submission Handoff

This file is a maintainer-facing checklist for the final CRAN submission. It is
excluded from the CRAN source package by `.Rbuildignore`.

## Current Submission Target

- Package: `mlmr`
- Version: `0.1.0`
- Maintainer: Marcus Harris <marcus.harris.uconn@gmail.com>
- Source tarball: `mlmr_0.1.0.tar.gz`
- GitHub prerelease for beta testers: `v0.1.0-alpha.2`
- Package site: <https://marcusharrisphd.com/mlmr/>

## Current Status

- CRAN package name check: `mlmr` was not found in current CRAN packages.
- Local full source-package check: passed with `0 errors | 0 warnings | 2 notes`
  on macOS when using Quarto's bundled Pandoc.
- Expected note: new submission.
- Local environment note: HTML validation was skipped because the available
  `tidy` executable was not recent enough. This is a local toolchain issue, not
  a package warning.
- GitHub Actions check matrix: passing on Windows release, macOS release,
  Ubuntu release, and Ubuntu devel.
- pkgdown deployment: passing.
- Installed-package smoke test: `mlmr::run_mlmr()` launches and responds locally.
- Automated app test: `tests/testthat/test-app.R` fits the example model in the
  bundled app and requires every output to render (skipped on CRAN).
- Reviewer-facing items addressed: title without "in R", method references
  with DOIs in the Description, unwrapped examples, absolute README links,
  `example_hsb()` restores the caller's random-number state.
- Still to run by the maintainer before upload: win-builder (R-devel and
  R-release) and, optionally, R-hub. Results are emailed to the maintainer.
- `citation("mlmr")`: works after installation.
- Public documentation scan: no local machine paths detected.
- Source tarball scan: no development folders, generated docs site, release
  notes, or local machine paths detected.

## Final Commands Before Upload

Run these from the package root immediately before CRAN submission:

```powershell
$env:PATH = "C:\Program Files\Quarto\bin\tools;" + $env:PATH
$env:RSTUDIO_PANDOC = "C:\Program Files\Quarto\bin\tools"
R CMD build .
R CMD check --as-cran mlmr_0.1.0.tar.gz
```

On macOS with Quarto installed in `/Applications/quarto`, the equivalent
commands are:

```sh
PATH=/Applications/quarto/bin/tools/aarch64:$PATH \
RSTUDIO_PANDOC=/Applications/quarto/bin/tools/aarch64 \
R CMD build .

PATH=/Applications/quarto/bin/tools/aarch64:$PATH \
RSTUDIO_PANDOC=/Applications/quarto/bin/tools/aarch64 \
R CMD check --as-cran mlmr_0.1.0.tar.gz
```

Optional manual check if LaTeX is configured:

```powershell
R CMD Rd2pdf . --output=dev/mlmr-reference-manual.pdf
```

If multiple LaTeX distributions are installed, make sure the working LaTeX
distribution comes first on `PATH` before building the manual.

## CRAN Submission Dry Run

Complete this dry run in a clean checkout immediately before uploading to CRAN:

1. Pull or clone a fresh copy of `main`.
2. Confirm the version in `DESCRIPTION`, `NEWS.md`, `cran-comments.md`, and this
   file all refer to the same release.
3. Run the local readiness script:

   ```r
   source("dev/check-publication-readiness.R")
   ```

4. Build the source package with Quarto's bundled Pandoc available on `PATH`.
5. Run `R CMD check --as-cran mlmr_0.1.0.tar.gz`.
6. Install the built tarball into a temporary library and verify:

   ```r
   library(mlmr)
   citation("mlmr")
   mlmr::mlm_validation_benchmarks(run = TRUE)
   ```

7. Launch the installed app with `mlmr::run_mlmr()`, fit the built-in example
   model, and export the Quarto report, R code, raw LaTeX, APA tables,
   publication bundle, and validation report.
8. Inspect `mlmr_0.1.0.tar.gz` and confirm it includes built vignettes in
   `inst/doc` but excludes development and article-preparation files such as
   `paper/`, `dev/`, `CITATION.cff`, and `codemeta.json`.
9. Re-check the GitHub Actions matrix and pkgdown deployment after pushing the
   final release commit.
10. Upload only if there are no errors, no warnings, and only expected notes.

## CRAN Upload

Use the CRAN submission form:

<https://cran.r-project.org/submit.html>

Upload:

```text
mlmr_0.1.0.tar.gz
```

Suggested optional comment:

```text
This is a new submission.

R CMD check was run locally and through GitHub Actions across Windows release,
macOS release, Ubuntu release, and Ubuntu devel. The only remaining note is the
expected "New submission" note.

mlmr provides a Shiny interface and R toolkit for fitting, understanding, and
reporting mixed-effects and multilevel models with lme4. The app is launched by
run_mlmr() and includes a built-in example dataset so users can demo the
workflow without external files.
```

After upload, watch for the CRAN confirmation email and confirm the submission.

## Pressure-Test Walkthrough Before Submission

1. Install the prerelease in a fresh R session:

   ```r
   remotes::install_github("MarcusHarrisUConn/mlmr@v0.1.0-alpha.2",
     upgrade = "never")
   ```

2. Launch the app:

   ```r
   mlmr::run_mlmr()
   ```

3. Fit the built-in example model.
4. Confirm the Results dashboard populates.
5. Review the Tables, Equations, Diagnostics, and Report & Code sections.
6. Export or copy:
   - APA fixed-effects table;
   - variance components table;
   - ICC table;
   - level-by-level equations;
   - combined equation;
   - raw LaTeX;
   - reproducible R code;
   - Quarto report text.
7. Verify the generated R code can reproduce the fitted model.
8. Confirm the app displays clear errors when an invalid custom model is chosen.
9. Review the public docs:
   - <https://marcusharrisphd.com/mlmr/>
   - <https://marcusharrisphd.com/mlmr/BETA_TESTING.html>
   - <https://marcusharrisphd.com/mlmr/articles/supported-models.html>

## Do Not Submit If

- `R CMD check --as-cran` has an error, warning, or unexplained note.
- The app fails to launch from an installed package.
- Public docs contain local machine paths or private information.
- The tarball includes development artifacts that should be excluded.
- The package name becomes unavailable before submission.
