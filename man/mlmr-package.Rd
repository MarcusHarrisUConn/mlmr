\docType{package}
\name{mlmr-package}
\alias{mlmr}
\alias{mlmr-package}
\title{A Guided Interface for Mixed-Effects and Multilevel Models}
\description{
  \pkg{mlmr} provides a guided level-by-level Shiny interface and supporting R
  toolkit for specifying, fitting, understanding, and reporting mixed-effects
  and multilevel models with \pkg{lme4}.
}
\details{
  The package is designed for researchers, instructors, and students who want a
  guided hierarchical equation workflow while preserving transparent and
  reproducible R code. It supports two- and three-level nested designs,
  longitudinal growth models, crossed random effects, generalized linear mixed
  models, grand-mean and cluster-mean centering, optional Satterthwaite and
  Kenward-Roger degrees of freedom, Huber-White cluster-robust sandwich standard
  errors (\code{\link{mlm_robust_se}}), multiparameter fixed-effect Wald tests
  (\code{\link{mlm_wald_test}}), single-term variance-component likelihood-ratio
  tests (\code{\link{mlm_variance_tests}}), Level-1 variance homogeneity testing
  (\code{\link{mlm_homogeneity_test}}), logistic population-average marginal
  effects (\code{\link{mlm_population_average}}), downloadable Level-1 and
  cluster residual files (\code{\link{mlm_residual_files}}), Nakagawa \eqn{R^2}
  (\code{\link{mlm_r2}}), empirical Bayes random-intercept and random-slope
  reliabilities (\code{\link{mlm_reliability}}), pre-fit and post-fit symbolic
  hierarchical equations (\code{\link{mlm_spec_equations}},
  \code{\link{mlm_latex_equations}}), APA-style tables, Tau covariance displays,
  diagnostics, and \code{.zip} publication archive bundles.

  Launch the app with \code{\link{run_mlmr}()}.

  Inspect current scope and validation evidence with
  \code{\link{mlm_supported_models}()}, \code{\link{mlm_feature_matrix}()}, and
  \code{\link{mlm_validation_report}()}.

  Create manuscript and reproducibility materials with
  \code{\link{mlm_publication_bundle}()} and
  \code{\link{mlm_write_publication_bundle}()}.
}
\seealso{
  \code{\link{run_mlmr}},
  \code{\link{example_hsb}},
  \code{\link{mlm_spec_equations}},
  \code{\link{mlm_robust_se}},
  \code{\link{mlm_r2}},
  \code{\link{mlm_supported_models}},
  \code{\link{mlm_validation_report}},
  \code{\link{mlm_publication_bundle}}
}
\author{
  Marcus Harris
}
\keyword{package}
