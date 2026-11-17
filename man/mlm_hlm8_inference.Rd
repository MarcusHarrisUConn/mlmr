\name{mlm_robust_se}
\alias{mlm_robust_se}
\alias{mlm_variance_tests}
\alias{mlm_homogeneity_test}
\alias{mlm_population_average}
\title{HLM-Style Robust Standard Errors, Variance Tests, and HGLM Marginal Effects}
\usage{
mlm_robust_se(result, type = c("CR1", "CR0"))

mlm_variance_tests(result)

mlm_homogeneity_test(result)

mlm_population_average(result)
}
\arguments{
\item{result}{A fitted \code{"mlm_result"} object returned by
\code{\link{mlm_fit}} (or a fitted \pkg{lme4} \code{merMod} object for
\code{mlm_robust_se()}, \code{mlm_homogeneity_test()}, and
\code{mlm_population_average()}).}
\item{type}{Cluster-robust variance estimator: \code{"CR1"} (default small-
sample \eqn{J / (J - 1)} correction) or \code{"CR0"} (unadjusted Huber-White
sandwich estimator matching classical HLM asymptotic output).}
}
\description{
Computes HLM 8-style inferential companions for multilevel models fit with
\pkg{lme4}: Huber-White cluster-robust sandwich standard errors, single-term
likelihood-ratio tests for variance components, Bartlett's \eqn{H} test of
Level-1 residual variance homogeneity across clusters, and population-average
marginal odds ratios for binary logit models.
}
\details{
\code{mlm_robust_se()} implements the empirical sandwich covariance estimator
of Liang and Zeger (1986) and Raudenbush and Bryk (2002, Eq. 9.15) clustered at
the highest grouping level. Comparing model-based and cluster-robust standard
errors provides a diagnostic check against covariance or distributional
misspecification when the number of top-level clusters is moderate to large.

\code{mlm_variance_tests()} refits reduced models dropping one random-effect
term at a time and computes likelihood-ratio \eqn{\chi^2} statistics with
boundary-adjusted mixture \eqn{p}-values (Stram & Lee, 1994).

\code{mlm_homogeneity_test()} computes Raudenbush and Bryk's (2002, pp.
263--265) \eqn{H} statistic testing equal within-cluster Level-1 residual
variance across clusters with at least two within-cluster degrees of freedom.

\code{mlm_population_average()} pairs unit-specific logistic HGLM coefficients
and odds ratios with Zeger et al.'s (1988) marginal population-average
attenuation factor across cluster intercept variance.
}
\value{
Each helper returns a data frame suitable for inspection, reporting, or
manuscript export.
}
\examples{
dat <- example_hsb(n_schools = 8, min_students = 10, max_students = 14)
spec <- mlm_spec(
  outcome = "mathscore",
  fixed = list(ses = list(center = "CWC"), meanses = list(center = "GMC")),
  grouping = list(schoolid = "schoolid"),
  random = list(schoolid = list(intercept = TRUE, slopes = "ses",
    correlation = TRUE)),
  data = dat
)
res <- suppressWarnings(mlm_fit(spec))
mlm_robust_se(res)
mlm_variance_tests(res)
mlm_homogeneity_test(res)
}
