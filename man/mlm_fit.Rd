\name{mlm_fit}
\alias{mlm_fit}
\title{Fit a Multilevel Model Specification}
\usage{
mlm_fit(spec, REML = TRUE, optimizer = "bobyqa", maxfun = 20000)
}
\arguments{
\item{spec}{An \code{"mlm_spec"} object created with \code{\link{mlm_spec}}.}
\item{REML}{Logical; whether to use restricted maximum likelihood for Gaussian
models.}
\item{optimizer}{Optimizer passed to \pkg{lme4}.}
\item{maxfun}{Maximum number of function evaluations.}
}
\description{
Centers requested predictors, builds an \pkg{lme4} formula, and fits the
specified mixed-effects or multilevel model.
}
\value{
A list containing the fitted model, formula, centered data, centering code, and
model specification.
}
\examples{
\donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  dat <- example_hsb(n_schools = 4, min_students = 5, max_students = 6)
  spec <- mlm_spec(
    outcome = "mathscore",
    fixed = list(ses = list(center = "CWC")),
    grouping = list(schoolid = "schoolid"),
    random = list(schoolid = list(intercept = TRUE, slopes = character(),
      correlation = TRUE)),
    data = dat
  )
  fit <- mlm_fit(spec)
  summary(fit$fit)
}
}
}
