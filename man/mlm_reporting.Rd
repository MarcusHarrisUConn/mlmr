\name{mlm_apa_tables}
\alias{mlm_apa_tables}
\alias{mlm_equations}
\alias{mlm_latex_equations}
\alias{mlm_spec_equations}
\alias{mlm_r2}
\alias{mlm_reliability}
\title{Create Tables, Effect Sizes, and Equations for mlmr Specifications and Fits}
\usage{
mlm_apa_tables(result, format = c("list", "html", "latex"),
  exponentiate = FALSE)

mlm_r2(result)

mlm_reliability(result)

mlm_equations(result)

mlm_latex_equations(result)

mlm_spec_equations(spec, data = spec$data)
}
\arguments{
\item{result}{A fitted result returned by \code{\link{mlm_fit}} (or a fitted
\pkg{lme4} \code{merMod} object for \code{mlm_r2()} and
\code{mlm_reliability()}).}
\item{format}{Output format for APA tables: \code{"list"}, \code{"html"}, or
\code{"latex"}. The list format returns individual data frames; HTML and LaTeX
formats return complete manuscript-oriented table documents.}
\item{exponentiate}{Logical; if \code{TRUE}, the fixed-effects table for a
generalized model with a logit or log link gains exponentiated coefficients and
their intervals: odds ratios (\code{OR}) for logit models, incidence rate
ratios (\code{IRR}) for Poisson and negative-binomial models, and
\code{exp(b)} for other log-link models.}
\item{spec}{An \code{"mlm_spec"} object created with \code{\link{mlm_spec}}.}
\item{data}{Data frame used to classify variable levels for pre-fit symbolic
equation previews in \code{mlm_spec_equations()}.}
}
\description{
Creates manuscript-oriented outputs and symbolic equation displays from
\pkg{mlmr} model specifications and fitted models, including APA-style tables,
marginal and conditional \eqn{R^2}, empirical Bayes random-coefficient
reliabilities (for both intercepts and slopes), and hierarchical equations.
}
\value{
\code{mlm_apa_tables()} returns a named list of tables, an HTML document, or a
LaTeX document. \code{mlm_r2()} returns a one-row data frame containing marginal
and conditional \eqn{R^2}. \code{mlm_reliability()} returns a data frame of
empirical Bayes reliabilities across clusters for each random intercept and
random slope in Gaussian models. \code{mlm_equations()} returns plain-text
equation descriptions. \code{mlm_latex_equations()} and
\code{mlm_spec_equations()} return lists containing level labels, plain-language
notes, level-by-level LaTeX equations, the combined equation, and Tau matrix
strings.
}
\examples{
dat <- example_hsb(n_schools = 6, min_students = 8, max_students = 10)
spec <- mlm_spec(
  outcome = "mathscore",
  fixed = list(ses = list(center = "CWC")),
  grouping = list(schoolid = "schoolid"),
  random = list(schoolid = list(intercept = TRUE, slopes = "ses",
    correlation = TRUE)),
  data = dat
)
mlm_spec_equations(spec)$equations
result <- suppressWarnings(mlm_fit(spec))
mlm_apa_tables(result)
mlm_r2(result)
mlm_reliability(result)
mlm_latex_equations(result)
}
