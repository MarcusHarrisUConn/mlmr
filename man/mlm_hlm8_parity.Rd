\name{mlm_hlm8_parity}
\alias{mlm_hlm8_parity}
\alias{mlm_validation_benchmarks}
\alias{mlm_validation_report}
\title{Inspect HLM-Style Feature Parity and Validation Benchmarks}
\usage{
mlm_hlm8_parity()

mlm_validation_benchmarks(run = FALSE, seed = 20260423, tolerance = 0.5)

mlm_validation_report(seed = 20260423, tolerance = 0.5)
}
\arguments{
\item{run}{Logical; if \code{TRUE}, fit each benchmark model and compare AIC,
fixed-effect estimates, standard errors, variance components, ICCs, and
exported R script round-trip reproducibility against reference values.}
\item{seed}{Numeric seed used to generate the deterministic example data.}
\item{tolerance}{Maximum acceptable absolute AIC difference when
\code{run = TRUE}.}
}
\description{
Summarizes \pkg{mlmr}'s progress toward an HLM-style graphical workflow and
provides deterministic benchmark specifications for regression validation.
}
\details{
\code{mlm_hlm8_parity()} maps common HLM 8 workflows to the current \pkg{mlmr}
support level, evidence, and next development step.

\code{mlm_validation_benchmarks()} builds deterministic specifications across
eight workflows: null, random-intercept, random-slope, cross-level interaction,
three-level nested, crossed, longitudinal growth, and binary logistic GLMM.
With \code{run = TRUE}, each model is estimated and checked against stored
reference estimates, standard errors, variances, ICCs, and round-trip R script
execution.

\code{mlm_validation_report()} fits the benchmark suite and returns the
validation summary, fixed effects, variance components, ICCs, \eqn{R^2}, and
diagnostics.
}
\value{
\code{mlm_hlm8_parity()} and \code{mlm_validation_benchmarks()} return data
frames. \code{mlm_validation_report()} returns a named list of data frames and
metadata.
}
\examples{
mlm_hlm8_parity()
mlm_validation_benchmarks()
}
