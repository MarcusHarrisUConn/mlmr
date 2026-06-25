# Release Documentation Notes

This file records documentation tasks that are useful for beta releases but do
not belong in the built R package.

## Screenshot Assets

Screenshots for the pkgdown site live in:

```text
pkgdown/assets/screenshots/
```

When `pkgdown::build_site()` runs, these files are copied to:

```text
docs/screenshots/
```

The public URLs therefore use:

```text
https://marcusharrisphd.com/mlmr/screenshots/<file-name>.png
```

Current screenshot set:

- `mlmr-data-overview.png`
- `mlmr-model-builder.png`
- `mlmr-results-dashboard.png`
- `mlmr-results-tables.png`
- `mlmr-results-equations.png`
- `mlmr-report-code.png`

## Manual Screenshot Checklist

If screenshots need to be updated manually, launch the app and capture these
screens after fitting the built-in example model:

1. Data > Overview
2. Model
3. Results > Dashboard
4. Results > Tables
5. Results > Equations
6. Report & Code

## Local PDF Reference Manual

CRAN creates the package PDF reference manual automatically from the `.Rd`
documentation after acceptance. To create the same kind of manual locally, make
sure a working LaTeX distribution is available on `PATH` and run:

```powershell
R CMD Rd2pdf . --output=dev/mlmr-reference-manual.pdf
```

The current local copy is:

```text
dev/mlmr-reference-manual.pdf
```
