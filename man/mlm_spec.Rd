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
\item{outcome}{Name of the outcome variable.}
\item{distribution}{Outcome distribution. The main current workflow uses
\code{"gaussian"}.}
\item{link}{Link function for generalized mixed models.}
\item{fixed}{Named list of fixed predictors. Each entry may include a
\code{center} value such as \code{"none"}, \code{"GMC"}, or \code{"CWC"}.}
\item{grouping}{Named list mapping conceptual grouping names to data columns.}
\item{nesting}{Optional text description of the nesting or design structure.}
\item{structure}{Model structure label, such as \code{"nested"} or
\code{"crossed"}.}
\item{random}{Named list describing random intercepts, slopes, and correlation
structure for each grouping factor.}
\item{interactions}{List of character vectors describing interaction terms.}
\item{predictor_levels}{Optional named list declaring conceptual predictor
levels.}
\item{data}{Data frame used to fit the model.}
}
\description{
Creates a structured model specification used by \code{\link{mlm_formula}},
\code{\link{mlm_fit}}, the Shiny app, and reporting helpers.
}
\value{
An object of class \code{"mlm_spec"}.
}
\examples{
dat <- example_hsb(n_schools = 4, min_students = 5, max_students = 6)
spec <- mlm_spec(
  outcome = "mathscore",
  fixed = list(ses = list(center = "CWC")),
  grouping = list(schoolid = "schoolid"),
  random = list(schoolid = list(intercept = TRUE, slopes = "ses",
    correlation = TRUE)),
  data = dat
)
mlm_formula(spec)
}
