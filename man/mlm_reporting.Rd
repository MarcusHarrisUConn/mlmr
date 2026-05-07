\name{mlm_apa_tables}
\alias{mlm_apa_tables}
\alias{mlm_equations}
\alias{mlm_latex_equations}
\title{Create Tables and Equations for a Fitted mlmr Model}
\usage{
mlm_apa_tables(result, format = c("list", "html", "latex"))

mlm_equations(result)

mlm_latex_equations(result)
}
\arguments{
\item{result}{A fitted result returned by \code{\link{mlm_fit}}.}
\item{format}{Output format for APA tables: \code{"list"}, \code{"html"}, or
\code{"latex"}. The list format returns individual data frames; HTML and LaTeX
formats return complete manuscript-oriented table documents.}
}
\description{
Creates manuscript-oriented outputs from a fitted \pkg{mlmr} model. These
helpers return APA-style tables, level-by-level equations, combined equations,
and LaTeX equation displays.
}
\value{
\code{mlm_apa_tables()} returns a named list of tables, an HTML document, or a
LaTeX document. \code{mlm_equations()} returns plain-text equation descriptions.
\code{mlm_latex_equations()} returns raw LaTeX equation components.
}
\details{
The fixed-effects table includes estimates, standard errors, large-sample test
statistics, p values, and Wald 95 percent confidence intervals. For Gaussian
mixed models fit with \pkg{lme4}, p values are large-sample normal
approximations and should be interpreted cautiously.

\code{mlm_latex_equations()} returns level-by-level equations, a combined full
equation, and Tau variance-covariance matrix displays. These outputs are meant
to support manuscript preparation and teaching; users should review notation and
substantive labels before publication.
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
  result <- mlm_fit(spec)
  mlm_apa_tables(result)
  mlm_latex_equations(result)
}
}
}
