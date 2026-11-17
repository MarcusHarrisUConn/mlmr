\name{run_mlmr}
\alias{run_mlmr}
\title{Launch the mlmr Shiny App}
\usage{
run_mlmr(launch.browser = TRUE, port = getOption("shiny.port", NULL),
  host = getOption("shiny.host", "127.0.0.1"))
}
\arguments{
\item{launch.browser}{Logical; whether to open the app in the default browser.}
\item{port}{Optional port passed to \code{shiny::runApp}.}
\item{host}{IPv4/IPv6 host interface passed to \code{shiny::runApp}, defaulting
to \code{"127.0.0.1"}.}
}
\description{
Launches the \pkg{mlmr} Shiny interface for mixed-effects and multilevel
modeling with \pkg{lme4}.
}
\details{
The app opens with built-in HSB-style educational and longitudinal growth
datasets so users can test the complete workflow before uploading their own
data. Uploaded data can be provided as CSV, TSV/TXT, Excel, SPSS, SAS, or Stata
files when the optional import packages are installed.

The app guides model specification, readiness validation, estimation,
diagnostics, equation display, APA-style tables, and reproducible exports.
Use \code{\link{mlm_supported_models}()} to inspect the current supported,
advanced, and planned scope.
}
\value{
Runs the Shiny application. This function is called for its side effect and does
not return a modeling object.
}
\examples{
if (interactive()) {
  run_mlmr()
}
}
\seealso{
  \code{\link{mlm_supported_models}},
  \code{\link{mlm_spec}},
  \code{\link{mlm_fit}}
}
