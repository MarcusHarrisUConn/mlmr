# mlmr Release Checklist

This checklist is for the public beta and later CRAN release process.

## Public Beta Release

- [x] Confirm `R CMD check --as-cran` passes locally.
- [x] Confirm GitHub Actions passes on `main`.
- [x] Build and publish the `pkgdown` documentation site.
- [x] Add a **Software** tab or section on <https://marcusharrisphd.com/>
      linking to the `mlmr` pkgdown site.
- [x] Confirm <https://marcusharrisphd.com/mlmr/> resolves after Pages deploys.
- [x] Review README installation and demo instructions.
- [ ] Review vignettes and beta testing guide in a browser.
- [x] Add screenshots or short demo GIFs to documentation.
- [x] Generate local PDF reference manual for documentation review.
- [ ] Confirm `mlmr::run_mlmr()` launches from an installed package.
- [ ] Create GitHub release `v0.1.0-alpha`.
- [ ] Confirm release notes accurately describe production, experimental, and
      out-of-scope features.
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
- [x] Run checks on Windows, macOS, Linux, and R-devel through GitHub Actions.
- [ ] Check reverse dependencies if any exist.
- [ ] Review CRAN Repository Policy.
- [ ] Confirm examples are fast, interactive examples are guarded, and vignettes
      are lightweight.
- [ ] Confirm no generated files, local paths, or development artifacts are in
      the built package.
- [ ] Confirm public documentation describes supported, experimental, planned,
      and out-of-scope model features.
- [ ] Submit to CRAN.
- [ ] Respond to CRAN maintainer confirmation email.
