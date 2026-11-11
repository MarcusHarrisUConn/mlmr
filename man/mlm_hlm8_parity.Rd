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
\item{run}{Logical; if \code{TRUE}, fit each benchmark model and report AIC
values. The default \code{FALSE} only reports formulas and readiness checks.}
\item{seed}{Numeric seed used to generate the deterministic example data.}
\item{tolerance}{Maximum acceptable absolute AIC difference when
\code{run = TRUE}.}
}
\description{
Summarizes \pkg{mlmr}'s progress toward an HLM-style graphical workflow and
provides lightweight benchmark model specifications for validation.
}
\details{
\code{mlm_hlm8_parity()} returns a table mapping common HLM 8 workflows to the
current \pkg{mlmr} support level, evidence, and next development step.

\code{mlm_validation_benchmarks()} builds deterministic model specifications
for core teaching and validation workflows: null, random-intercept,
random-slope, cross-level interaction, and three-level models. With
\code{run = TRUE}, the helper fits each model with \code{\link{mlm_fit}()} and
compares the resulting AIC with stored deterministic reference values.

\code{mlm_validation_report()} fits the benchmark suite and returns a named
list containing the validation summary, fixed effects, variance components,
ICCs, and diagnostics. This is intended as reviewer-facing evidence for CRAN,
software-paper, and classroom validation workflows.
}
\value{
\code{mlm_hlm8_parity()} and \code{mlm_validation_benchmarks()} return data
frames. \code{mlm_validation_report()} returns a named list of data frames and
metadata.
}
\examples{
mlm_hlm8_parity()
mlm_validation_benchmarks()
\donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  mlm_validation_benchmarks(run = TRUE)
  names(mlm_validation_report())
}
}
}
