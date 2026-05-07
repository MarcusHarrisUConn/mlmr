\name{mlm_formula}
\alias{mlm_formula}
\title{Build an lme4 Formula From an mlmr Specification}
\usage{
mlm_formula(spec)
}
\arguments{
\item{spec}{An \code{"mlm_spec"} object created with \code{\link{mlm_spec}}.}
}
\description{
Converts an \pkg{mlmr} model specification into an \pkg{lme4}-compatible model
formula, including centered predictor names, interactions, and random effects.
}
\value{
A model formula.
}
\details{
The formula uses the effective predictor names created by the centering
choices. For example, a predictor with \code{center = "CWC"} appears with a
\code{_CWC} suffix. Correlated random slopes are represented with
\code{(1 + slope | group)} syntax, whereas independent random slopes are
represented with separate \code{(1 | group)} and \code{(0 + slope | group)}
terms.
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
