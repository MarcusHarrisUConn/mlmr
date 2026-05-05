## R CMD check results

0 errors | 0 warnings | 2 notes

## Test environments

- Local Windows 11, R 4.5.1
- GitHub Actions, Windows latest, R release

## Check notes

- CRAN incoming reports "New submission"; this is expected for the first CRAN
  submission.
- The local Windows check reported "unable to verify current time" while
  checking for future file timestamps. This appears to be an environment/time
  verification note rather than a package issue.

## Downstream dependencies

There are no known downstream dependencies.

## Submission notes

This is a new submission.

`mlmr` provides a Shiny interface and R toolkit for fitting, understanding, and
reporting mixed-effects and multilevel models with `lme4`. The Shiny app is
launched by `run_mlmr()` and includes a built-in example dataset so users can
demo the workflow without external files.

The package includes optional upload support for Excel, SPSS, SAS, and Stata
files through packages listed in `Suggests`.
