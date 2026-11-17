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
  while preserving transparent and reproducible R code. It supports two- and
  three-level nested designs, longitudinal growth models, crossed random
  effects, grand-mean and cluster-mean centering, optional Satterthwaite and
  Kenward-Roger degrees of freedom, Nakagawa \eqn{R^2}, random-intercept
  reliability, APA-style tables, level-by-level and combined equations, Tau
  variance-covariance displays, diagnostics, and \code{.zip} publication
  archive bundles.

  Launch the app with \code{\link{run_mlmr}()}.

  Inspect current scope and validation evidence with
  \code{\link{mlm_supported_models}()}, \code{\link{mlm_hlm8_parity}()}, and
  \code{\link{mlm_validation_report}()}.

  Create manuscript and reproducibility materials with
  \code{\link{mlm_publication_bundle}()} and
  \code{\link{mlm_write_publication_bundle}()}.
}
\seealso{
  \code{\link{run_mlmr}},
  \code{\link{example_hsb}},
  \code{\link{mlm_r2}},
  \code{\link{mlm_supported_models}},
  \code{\link{mlm_validation_report}},
  \code{\link{mlm_publication_bundle}}
}
\author{
  Marcus Harris
}
\keyword{package}
