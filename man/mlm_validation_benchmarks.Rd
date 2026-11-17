\name{mlm_validation_benchmarks}
\alias{mlm_feature_matrix}
\alias{mlm_validation_benchmarks}
\alias{mlm_validation_report}
\title{Inspect Multilevel Capabilities and Validation Benchmarks}
\usage{
mlm_feature_matrix()

mlm_validation_benchmarks(run = FALSE, seed = 20260423, tolerance = 0.5)

mlm_validation_report(seed = 20260423, tolerance = 0.5)
}
\arguments{
\item{run}{Logical; if \code{TRUE}, fit each benchmark model and verify AICs,
fixed-effect estimates, standard errors, variance components, ICCs, and
round-trip R code execution. The default \code{FALSE} checks formulas and
readiness checks without fitting.}
\item{seed}{Numeric seed used to generate the deterministic example data.}
\item{tolerance}{Maximum acceptable absolute AIC difference when
\code{run = TRUE}.}
}
\description{
Summarizes \pkg{mlmr}'s multilevel modeling capabilities and provides
deterministic benchmark model specifications for validation.
}
\details{
\code{mlm_feature_matrix()} returns a table summarizing \pkg{mlmr}'s modeling
capabilities, support status, implementation details, and planned extensions.

\code{mlm_validation_benchmarks()} builds deterministic model specifications
across eight core workflows: null, random-intercept, random-slope, cross-level
interaction, three-level nested, cross-classified, longitudinal growth, and
binary logistic mixed models. With \code{run = TRUE}, the helper fits each
model with \code{\link{mlm_fit}()}, compares fitted metrics against stored
reference values, and verifies that the exported R code reproduces the fitted
coefficients.

\code{mlm_validation_report()} fits the benchmark suite and returns a named
list containing the validation summary, fixed effects, variance components,
ICCs, \eqn{R^2}, and diagnostics.
}
\value{
\code{mlm_feature_matrix()} and \code{mlm_validation_benchmarks()} return data
frames. \code{mlm_validation_report()} returns a named list of data frames and
metadata.
}
\examples{
mlm_feature_matrix()
mlm_validation_benchmarks()
}
