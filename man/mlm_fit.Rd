\name{mlm_fit}
\alias{mlm_fit}
\alias{print.mlm_result}
\title{Fit a Multilevel Model Specification}
\usage{
mlm_fit(spec, REML = TRUE, optimizer = "bobyqa", maxfun = 20000)

\method{print}{mlm_result}(x, ...)
}
\arguments{
\item{spec}{An \code{"mlm_spec"} object created with \code{\link{mlm_spec}}.}
\item{REML}{Logical; whether to use restricted maximum likelihood for Gaussian
models. Use \code{REML = FALSE} when comparing nested models that differ in
fixed effects.}
\item{optimizer}{Optimizer passed to \pkg{lme4}, such as \code{"bobyqa"},
\code{"Nelder_Mead"}, or \code{"nloptwrap"}.}
\item{maxfun}{Maximum number of function evaluations.}
\item{x}{An \code{"mlm_result"} object returned by \code{mlm_fit()}.}
\item{...}{Ignored.}
}
\description{
Centers requested predictors, builds an \pkg{lme4} formula, and fits the
specified mixed-effects or multilevel model.
}
\value{
An object of class \code{"mlm_result"}: a list with the fitted \pkg{lme4}
model (\code{fit}), the model formula (\code{formula}), the data including
centered predictors (\code{data}), R code that recreates the centered
predictors (\code{centering_code}), the model specification (\code{spec}), and
the estimation settings (\code{settings}: \code{REML}, \code{optimizer}, and
\code{maxfun}). Printing the object shows a compact summary with the
fixed-effects table.
}
\details{
For Gaussian models, \code{mlm_fit()} calls \code{lme4::lmer()}. For supported
generalized mixed models, it calls \code{lme4::glmer()} or
\code{lme4::glmer.nb()}. The returned \code{fit} element is the underlying
\pkg{lme4} model object, so users can pass it to standard \pkg{lme4} methods.

The function performs requested grand-mean and cluster-mean centering before
fitting. Centering uses the analysis sample: means are computed from the rows
that are complete on every model variable, which are the rows \pkg{lme4} keeps.
A grand-mean-centered predictor therefore has a mean of zero, and a
cluster-mean-centered predictor has cluster means of zero, among the
observations that enter the model. Rows with missing values stay in the
returned \code{data} but are not used for estimation.

\code{mlm_fit()} stops with an explanatory message when a specification cannot
be fit as requested, for example when a categorical predictor is centered, a
random slope is requested for a variable that is not a fixed predictor, or no
random effects are specified. Users should inspect convergence messages, singular fits, residual
patterns, and whether the random-effects structure is supported by the data.
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
result
summary(result$fit)
}
