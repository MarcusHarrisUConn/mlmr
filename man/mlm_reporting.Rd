\name{mlm_apa_tables}
\alias{mlm_apa_tables}
\alias{mlm_equations}
\alias{mlm_latex_equations}
\alias{mlm_r2}
\alias{mlm_reliability}
\title{Create Tables, Effect Sizes, and Equations for a Fitted mlmr Model}
\usage{
mlm_apa_tables(result, format = c("list", "html", "latex"),
  exponentiate = FALSE)

mlm_r2(result)

mlm_reliability(result)

mlm_equations(result)

mlm_latex_equations(result)
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
}
\description{
Creates manuscript-oriented outputs from a fitted \pkg{mlmr} model, including
APA-style tables, marginal and conditional \eqn{R^2}, HLM-style random-intercept
reliability summaries, level-by-level equations, and combined equations.
}
\value{
\code{mlm_apa_tables()} returns a named list of tables, an HTML document, or a
LaTeX document. \code{mlm_r2()} returns a one-row data frame containing marginal
and conditional \eqn{R^2} and variance components. \code{mlm_reliability()}
returns a data frame of empirical Bayes random-intercept reliabilities across
clusters for Gaussian models. \code{mlm_equations()} returns plain-text equation
descriptions. \code{mlm_latex_equations()} returns a list with level labels
(\code{level}), plain-language notes (\code{level_notes}), one LaTeX string per
level (\code{equations}), the combined equation (\code{combined}), one Tau
matrix display per grouping factor (\code{tau}), a coefficient key (\code{key}),
the notation used (\code{notation}), and a summary note (\code{note}).
}
\details{
When \code{result} was fit with Satterthwaite or Kenward-Roger degrees of
freedom, \code{mlm_apa_tables()} includes a \code{df} column and \eqn{t}-based
intervals. \code{mlm_r2()} implements the variance-partitioning framework of
Nakagawa, Johnson, and Schielzeth (2017) with random-slope variance contributions
from Johnson (2014). \code{mlm_reliability()} computes the mean and range of
cluster-specific random-intercept reliabilities following Raudenbush and Bryk
(2002).
}
\examples{
dat <- example_hsb(n_schools = 4, min_students = 5, max_students = 6)
spec <- mlm_spec(
  outcome = "mathscore",
  fixed = list(ses = list(center = "CWC")),
  grouping = list(schoolid = "schoolid"),
  random = list(schoolid = list(intercept = TRUE, slopes = character(),
    correlation = TRUE)),
  data = dat
)
result <- mlm_fit(spec)
mlm_apa_tables(result)
mlm_r2(result)
mlm_reliability(result)
mlm_latex_equations(result)
}
