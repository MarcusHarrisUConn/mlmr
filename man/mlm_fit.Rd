\name{mlm_fit}
\alias{mlm_fit}
\title{Fit a Multilevel Model Specification}
\usage{
mlm_fit(spec, REML = TRUE, optimizer = "bobyqa", maxfun = 20000)
}
\arguments{
\item{spec}{An \code{"mlm_spec"} object created with \code{\link{mlm_spec}}.}
\item{REML}{Logical; whether to use restricted maximum likelihood for Gaussian
models. Use \code{REML = FALSE} when comparing nested models that differ in
fixed effects.}
\item{optimizer}{Optimizer passed to \pkg{lme4}, such as \code{"bobyqa"},
\code{"Nelder_Mead"}, or \code{"nloptwrap"}.}
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
\details{
For Gaussian models, \code{mlm_fit()} calls \code{lme4::lmer()}. For supported
generalized mixed models, it calls \code{lme4::glmer()} or
\code{lme4::glmer.nb()}. The returned \code{fit} element is the underlying
\pkg{lme4} model object, so users can pass it to standard \pkg{lme4} methods.

The function performs requested grand-mean and cluster-mean centering before
fitting. Users should inspect convergence messages, singular fits, residual
patterns, and whether the random-effects structure is supported by the data.
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
