\docType{package}
\name{mlmr-package}
\alias{mlmr}
\alias{mlmr-package}
\title{A Guided Interface for Mixed-Effects and Multilevel Models}
\description{
  \pkg{mlmr} provides a guided Shiny interface and supporting R toolkit for
  fitting, understanding, and reporting mixed-effects and multilevel models
  with \pkg{lme4}.
}
\details{
  The package is designed for users who want an HLM-style graphical workflow
  while preserving transparent and reproducible R code. The app supports
  level-based predictor selection, centering decisions, fixed effects,
  random intercepts, random slopes, interactions, model diagnostics,
  APA-style tables, level-by-level equations, combined equations, Tau
  variance-covariance displays, and reproducible exports.

  Launch the app with \code{\link{run_mlmr}()}.

  Inspect the current production scope with
  \code{\link{mlm_supported_models}()}. Inspect HLM-style parity and validation
  evidence with \code{\link{mlm_hlm8_parity}()} and
  \code{\link{mlm_validation_report}()}.

  Create manuscript and reproducibility materials with
  \code{\link{mlm_publication_bundle}()}.

  To cite the package, use \code{citation("mlmr")}.
}
\seealso{
  \code{\link{run_mlmr}},
  \code{\link{mlm_supported_models}},
  \code{\link{mlm_hlm8_parity}},
  \code{\link{mlm_validation_report}},
  \code{\link{mlm_publication_bundle}}
}
\author{
  Marcus Harris
}
\keyword{package}
