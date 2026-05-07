\name{mlm_spec}
\alias{mlm_spec}
\title{Create a Multilevel Model Specification}
\usage{
mlm_spec(outcome, distribution = "gaussian", link = "identity",
  fixed = list(), grouping = list(), nesting = NULL, structure = "nested",
  random = list(), interactions = list(), predictor_levels = list(),
  data = NULL)
}
\arguments{
\item{outcome}{Character string giving the name of the outcome variable in
\code{data}.}
\item{distribution}{Outcome distribution. The primary production workflow uses
\code{"gaussian"}. Advanced workflows may use \code{"binomial"},
\code{"poisson"}, \code{"negative binomial"}, or \code{"Gamma"}.}
\item{link}{Link function for generalized mixed models. For Gaussian models the
default \code{"identity"} link is used.}
\item{fixed}{Named list of fixed predictors. Each entry may include a
\code{center} value: \code{"none"}, \code{"GMC"} for grand-mean centering, or
\code{"CWC"} for cluster-mean centering.}
\item{grouping}{Named list mapping conceptual grouping names to data columns.
For example, \code{list(schoolid = "schoolid")} or
\code{list(schoolid = "schoolid", districtid = "districtid")}.}
\item{nesting}{Optional text description of the nesting or design structure.}
\item{structure}{Model structure label, such as \code{"nested"} or
\code{"crossed"}. This label is used for documentation and guidance; the fitted
model is determined by the grouping and random-effect terms.}
\item{random}{Named list describing random intercepts, slopes, and correlation
structure for each grouping factor. Each entry may contain
\code{intercept = TRUE}, \code{slopes = c(...)}, and
\code{correlation = TRUE} or \code{FALSE}.}
\item{interactions}{List of character vectors describing interaction terms,
for example \code{list(c("ses", "sector"))}.}
\item{predictor_levels}{Optional named list declaring conceptual predictor
levels, such as \code{list(level1 = c("ses"), level2 = c("meanses"))}. This is
used for app display and reporting.}
\item{data}{Data frame used to fit the model.}
}
\description{
Creates a structured model specification used by \code{\link{mlm_formula}},
\code{\link{mlm_fit}}, the Shiny app, and reporting helpers.
}
\value{
An object of class \code{"mlm_spec"}, represented as a structured list. It can
be passed to \code{\link{mlm_formula}} or \code{\link{mlm_fit}}.
}
\details{
\code{mlm_spec()} records the modeling choices made in the app or in code. It
does not fit the model by itself. Centered variables are created during
\code{\link{mlm_fit}} and are reflected in the generated \pkg{lme4} formula.

Users are responsible for confirming that grouping IDs, predictor levels,
centering decisions, interactions, and random effects match the research design.
}
\examples{
dat <- example_hsb(n_schools = 4, min_students = 5, max_students = 6)
spec <- mlm_spec(
  outcome = "mathscore",
  fixed = list(
    ses = list(center = "CWC"),
    meanses = list(center = "GMC"),
    sector = list(center = "none")
  ),
  grouping = list(schoolid = "schoolid"),
  random = list(schoolid = list(intercept = TRUE, slopes = "ses",
    correlation = TRUE)),
  interactions = list(c("ses", "sector")),
  predictor_levels = list(level1 = "ses", level2 = c("meanses", "sector")),
  data = dat
)
mlm_formula(spec)
}
