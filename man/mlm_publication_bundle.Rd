\name{mlm_publication_bundle}
\alias{mlm_reproducibility_check}
\alias{mlm_methods_text}
\alias{mlm_publication_bundle}
\title{Create Publication and Reproducibility Materials}
\usage{
mlm_reproducibility_check(result, REML = NULL, optimizer = NULL,
  maxfun = NULL)

mlm_methods_text(result, REML = NULL, optimizer = NULL, maxfun = NULL)

mlm_publication_bundle(result, REML = NULL, optimizer = NULL,
  maxfun = NULL)
}
\arguments{
\item{result}{A fitted result returned by \code{\link{mlm_fit}}.}
\item{REML, optimizer, maxfun}{Estimation settings to report. These are
normally left as \code{NULL}: the estimator is read from the fitted model and
the optimizer settings from the values recorded by \code{\link{mlm_fit}}, so
the reported settings cannot contradict the model. The arguments are used only
for results that do not carry recorded settings.}
}
\description{
Creates publication-oriented reproducibility materials for a fitted
\pkg{mlmr} model.
}
\details{
\code{mlm_reproducibility_check()} returns a checklist covering the formula,
data footprint, estimator settings, centering decisions, random-effects
structure, convergence, singularity, reporting outputs, and software versions.

The reproducible R code in the bundle is a complete script: given the original
data frame as \code{dat}, it selects the analysis sample, recreates the
centered predictors, and refits the same model.

\code{mlm_methods_text()} returns copy-ready prose that should be reviewed and
adapted before publication. \code{mlm_publication_bundle()} combines the
methods text, reproducibility checklist, software statement, formula, APA
tables, equations, and reproducible R code into a named list.
}
\value{
\code{mlm_reproducibility_check()} returns a data frame.
\code{mlm_methods_text()} returns a character string.
\code{mlm_publication_bundle()} returns a named list of publication materials.
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
mlm_reproducibility_check(result)
mlm_methods_text(result)
names(mlm_publication_bundle(result))
}
