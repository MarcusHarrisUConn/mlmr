# mlmr Release Checklist

This checklist is for the public beta and later CRAN release process.

## Public Beta Release

- [ ] Confirm `R CMD check --as-cran` passes locally.
- [ ] Confirm GitHub Actions passes on `main`.
- [ ] Build and publish the `pkgdown` documentation site.
- [ ] Review README installation and demo instructions.
- [ ] Review vignettes in a browser.
- [ ] Add screenshots or short demo GIFs to documentation.
- [ ] Confirm `mlmr::run_mlmr()` launches from an installed package.
- [ ] Create GitHub release `v0.2.0-beta`.
- [ ] Share install instructions with beta testers:

```r
install.packages("pak")
pak::pak("MarcusHarrisUConn/mlmr")
mlmr::run_mlmr()
```

## CRAN Release

- [ ] Confirm package name availability immediately before submission.
- [ ] Update `cran-comments.md` with current check environments.
- [ ] Run `R CMD check --as-cran` locally.
- [ ] Run checks on Windows, macOS, and Linux.
- [ ] Check reverse dependencies if any exist.
- [ ] Review CRAN Repository Policy.
- [ ] Confirm examples are fast, interactive examples are guarded, and vignettes
      are lightweight.
- [ ] Confirm no generated files, local paths, or development artifacts are in
      the built package.
- [ ] Submit to CRAN.
- [ ] Respond to CRAN maintainer confirmation email.
