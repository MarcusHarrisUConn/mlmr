\name{run_mlmr}
\alias{run_mlmr}
\title{Launch the mlmr Shiny App}
\usage{
run_mlmr(launch.browser = TRUE, port = getOption("shiny.port", NULL))
}
\arguments{
\item{launch.browser}{Logical; whether to open the app in the default browser.}
\item{port}{Optional port passed to \code{shiny::runApp}.}
}
\description{
Launches the \pkg{mlmr} Shiny interface for mixed-effects and multilevel
modeling with \pkg{lme4}.
}
\details{
The app opens with a built-in HSB-style example dataset and a preset model so
users can test the complete workflow before uploading their own data. Uploaded
data can be provided as CSV, TSV/TXT, Excel, SPSS, SAS, or Stata files when the
optional import packages are installed.
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
