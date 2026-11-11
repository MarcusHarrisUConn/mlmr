\name{mlm_apa_tables}
\alias{mlm_apa_tables}
\alias{mlm_equations}
\alias{mlm_latex_equations}
\title{Create Tables and Equations for a Fitted mlmr Model}
\usage{
mlm_apa_tables(result, format = c("list", "html", "latex"),
  exponentiate = FALSE)

mlm_equations(result)

mlm_latex_equations(result)
}
\arguments{
\item{result}{A fitted result returned by \code{\link{mlm_fit}}.}
\item{format}{Output format for APA tables: \code{"list"}, \code{"html"}, or
\code{"latex"}. The list format returns individual data frames; HTML and LaTeX
formats return complete manuscript-oriented table documents.}
\item{exponentiate}{Logical; if \code{TRUE}, the fixed-effects table for a
generalized model with a logit or log link gains exponentiated coefficients and
their intervals: odds ratios (\code{OR}) for logit models, incidence rate
ratios (\code{IRR}) for Poisson and negative-binomial models, and
\code{exp(b)} for other log-link models. Ignored for Gaussian models and other
links.}
}
\description{
Creates manuscript-oriented outputs from a fitted \pkg{mlmr} model. These
helpers return APA-style tables, level-by-level equations, combined equations,
and LaTeX equation displays.
}
\value{
\code{mlm_apa_tables()} returns a named list of tables, an HTML document, or a
LaTeX document. \code{mlm_equations()} returns plain-text equation descriptions.
\code{mlm_latex_equations()} returns a list with the level labels
(\code{level}), one LaTeX string per level (\code{equations}), the combined
equation (\code{combined}), one Tau matrix display per grouping factor
(\code{tau}), a coefficient key linking each equation symbol to a fixed-effect
estimate (\code{key}), the notation used (\code{notation}), and a short
explanatory note (\code{note}).
}
\details{
The fixed-effects table includes estimates, standard errors, large-sample test
statistics, p values, and Wald 95 percent confidence intervals. For Gaussian
mixed models fit with \pkg{lme4}, p values are large-sample normal
approximations rather than denominator-degrees-of-freedom tests. For generalized
mixed models, p values and intervals are Wald approximations on the link scale.
Treat these values as screening evidence unless a confirmatory inferential
method is selected and documented.

Classical ICC tables are returned for Gaussian mixed models. For generalized
mixed models, \pkg{mlmr} suppresses classical ICC output and asks users to choose
a model-family-specific latent- or observation-scale ICC method before reporting
ICC values.

\code{mlm_latex_equations()} writes two- and three-level nested models in
hierarchical (level-by-level) notation. Predictors are assigned to levels from
the fitted data: a predictor that is constant within every unit of a grouping
factor is treated as measured at that level. Level-1 predictors appear in the
Level-1 equation, higher-level predictors appear in the intercept equation, and
cross-level interactions appear in the equation for the lower-level slope they
moderate, so \eqn{\gamma_{11}} is the effect of the first Level-2 predictor on
the first Level-1 slope. Equations are built from the fitted coefficients, so
each dummy code of a categorical predictor has its own coefficient and the
indices of the random effects in the Tau matrices match the equations.
Three-level models use \eqn{\pi}, \eqn{\beta}, and \eqn{\gamma} coefficients.
Generalized models are written for the linear predictor \eqn{\eta} with the
link function shown and no Level-1 residual. Models with crossed grouping
factors, or more than three levels, are written as a single mixed-model
equation instead. These outputs are meant to support manuscript preparation
and teaching; users should review notation and substantive labels before
publication.
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
mlm_latex_equations(result)
}
