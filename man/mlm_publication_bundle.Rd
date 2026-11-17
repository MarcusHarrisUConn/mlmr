\name{mlm_publication_bundle}
\alias{mlm_reproducibility_check}
\alias{mlm_methods_text}
\alias{mlm_publication_bundle}
\alias{mlm_write_publication_bundle}
\title{Create Publication and Reproducibility Materials}
\usage{
mlm_reproducibility_check(result, REML = NULL, optimizer = NULL,
  maxfun = NULL)

mlm_methods_text(result, REML = NULL, optimizer = NULL, maxfun = NULL)

mlm_publication_bundle(result, REML = NULL, optimizer = NULL,
  maxfun = NULL)

mlm_write_publication_bundle(result, file, REML = NULL, optimizer = NULL,
  maxfun = NULL)
}
\arguments{
\item{result}{A fitted result returned by \code{\link{mlm_fit}}.}
\item{file}{Destination path ending in \code{.zip} for
\code{mlm_write_publication_bundle()}.}
\item{REML, optimizer, maxfun}{Estimation settings to report. Normally left as
\code{NULL} so settings are read directly from the fitted model.}
}
\description{
Creates publication-oriented reproducibility materials and downloadable archive
bundles for a fitted \pkg{mlmr} model.
}
\details{
\code{mlm_reproducibility_check()} returns a checklist covering the formula,
data footprint, estimator and degrees-of-freedom settings, centering decisions,
random-effects structure, convergence, singularity, reporting outputs, and
software versions.

\code{mlm_methods_text()} returns copy-ready prose to review and adapt.
\code{mlm_publication_bundle()} combines methods text, checklist, software
statement, formula, APA tables, equations, and reproducible R code into a named
list. \code{mlm_write_publication_bundle()} writes a complete \code{.zip}
archive containing the Quarto report, standalone R script, LaTeX file, HTML APA
tables, checklist CSV, and software versions CSV.
}
\value{
\code{mlm_reproducibility_check()} returns a data frame.
\code{mlm_methods_text()} returns a character string.
\code{mlm_publication_bundle()} returns a named list of publication materials.
\code{mlm_write_publication_bundle()} invisibly returns the path to the created
\code{.zip} archive.
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
