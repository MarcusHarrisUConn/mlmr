## R CMD check results

0 errors | 0 warnings | 1 note

## Test environments

- win-builder (Windows Server 2022 x64, x86_64-w64-mingw32), R 4.6.2 ucrt (release) and R-devel
- Local macOS Tahoe 26.6.2 (aarch64-apple-darwin23), R 4.6.1
- GitHub Actions: windows-latest (R release), macos-latest (R release), ubuntu-latest (R release), ubuntu-latest (R devel)

## Check notes

- CRAN incoming feasibility reports `New submission`; this is expected for the
  initial CRAN release of `mlmr`.
- The words flagged under `Possibly misspelled words in DESCRIPTION` (`APA`,
  `Bolker`, `Brockhoff`, `Bryk`, `Enders`, `Kenward`, `Kuznetsova`, `Maechler`,
  `Nakagawa`, `Raudenbush`, `Satterthwaite`, `Schielzeth`, `Tofighi`) are
  author surnames and the standard APA acronym; all are spelled intentionally,
  and software/package names (`'Shiny'`, `'lme4'`, `'lmerTest'`) are
  single-quoted with DOIs/ISBNs.
- All `.Rd` files include explicit `\value` sections and unwrapped `< 1s`
  `\examples` (`run_mlmr()` is guarded by `if (interactive())`), and calling
  `example_hsb()` or `example_longitudinal()` restores the caller's `.Random.seed`
  via `on.exit()`.

## Downstream dependencies

There are no downstream dependencies (new submission).
