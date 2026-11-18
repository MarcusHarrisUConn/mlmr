\name{mlm_coef_plot}
\alias{mlm_coef_plot}
\alias{mlm_interaction_plot}
\alias{mlm_compare_table}
\title{Plot Multilevel Fixed Effects, Simple Slopes, and Side-by-Side Model Comparisons}
\usage{
mlm_coef_plot(result, include_intercept = FALSE)

mlm_interaction_plot(result, focal = NULL, moderator = NULL,
  vcov_type = c("CR1", "model"))

mlm_compare_table(models, digits = 2)
}
\arguments{
\item{result}{A fitted \code{"mlm_result"} object returned by
\code{\link{mlm_fit}} (or a fitted \pkg{lme4} \code{merMod} object).}
\item{include_intercept}{Logical; if \code{TRUE}, include the model intercept in
the fixed-effects forest plot.}
\item{focal}{Optional focal predictor name placed on the horizontal axis in
\code{mlm_interaction_plot()}. Defaults to the first active interaction term or
first fixed predictor.}
\item{moderator}{Optional moderating predictor name defining simple-slope lines
(factor levels or Mean \eqn{\pm 1} SD for numeric moderators), or
\code{"none"} for an unmoderated marginal trajectory.}
\item{vcov_type}{Covariance matrix used for 95\% confidence ribbons in
\code{mlm_interaction_plot()}: \code{"CR1"} (cluster-robust sandwich) or
\code{"model"} (model-based GLS/ML covariance).}
\item{models}{A named list of \code{"mlm_result"} objects (for example,
unconditional null, Level-1, and full models) passed to
\code{mlm_compare_table()}.}
\item{digits}{Integer number of decimal places for coefficients, standard
errors, and variance components in \code{mlm_compare_table()}.}
}
\description{
Creates publication-ready \pkg{ggplot2} visualizations of fixed-effect
coefficients and simple-slopes moderation trajectories, as well as side-by-side
multi-model APA comparison tables.
}
\value{
\code{mlm_coef_plot()} and \code{mlm_interaction_plot()} return \code{ggplot}
objects. \code{mlm_compare_table()} returns a data frame summarizing fixed
effects, variance components, sample sizes, deviance, information criteria, and
\eqn{R^2} side-by-side across models.
}
\examples{
dat <- example_hsb(n_schools = 8, min_students = 10, max_students = 14)
spec <- mlm_spec(
  outcome = "mathscore",
  fixed = list(ses = list(center = "CWC"), sector = list(center = "none")),
  grouping = list(schoolid = "schoolid"),
  random = list(schoolid = list(intercept = TRUE, slopes = "ses")),
  interactions = list(c("ses", "sector")),
  data = dat
)
res <- suppressWarnings(mlm_fit(spec))
p1 <- mlm_coef_plot(res)
p2 <- mlm_interaction_plot(res, focal = "ses", moderator = "sector")
mlm_compare_table(list("Full Model" = res))
}
