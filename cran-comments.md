## R CMD check results

0 errors | 0 warnings | 1 expected note on GitHub Actions CI (Windows, macOS, Ubuntu)
0 errors | 0 warnings | 2 notes on local macOS

## Test environments

- Local macOS Tahoe 26.6.2 (aarch64-apple-darwin23), R 4.6.1
- GitHub Actions: windows-latest (R release)
- GitHub Actions: macos-latest (R release)
- GitHub Actions: ubuntu-latest (R release)
- GitHub Actions: ubuntu-latest (R devel)

## Check notes

- CRAN incoming feasibility reports `New submission`; this is expected for the
  initial CRAN release of `mlmr`.
- Proper names and standard statistical acronyms in `DESCRIPTION` (`Bates`,
  `Maechler`, `Bolker`, `Enders`, `Tofighi`, `Satterthwaite`, `Kenward`,
  `Kuznetsova`, `Brockhoff`, `Christensen`, `Huber`, `Raudenbush`, `Bryk`,
  `Nakagawa`, `Schielzeth`, `APA`) are spelled intentionally; software and
  package names (`'Shiny'`, `'lme4'`, `'lmerTest'`) are single-quoted and
  referenced with DOIs/ISBNs.
- Local macOS check reports that HTML validation was skipped because the local
  `tidy` executable is older than HTML Tidy 5; full HTML checks run cleanly on
  the GitHub Actions CI matrix.
- All `\Rd` files include explicit `\value` tags and unwrapped `< 1s`
  `\examples` (`run_mlmr()` is guarded by `if (interactive())`), and calling
  `example_hsb()` or `example_longitudinal()` restores the caller's `.Random.seed`
  via `on.exit()`.

## Downstream dependencies

There are no downstream dependencies (new submission).
