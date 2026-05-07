\docType{package}
\name{mlmr-package}
\alias{mlmr}
\alias{mlmr-package}
\title{Mixed-Effects and Multilevel Models in R}
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
  \code{\link{mlm_supported_models}()}.

  To cite the package, use \code{citation("mlmr")}.
}
\seealso{
  \code{\link{run_mlmr}},
  \code{\link{mlm_supported_models}}
}
\author{
  Marcus Harris
}
\keyword{package}
