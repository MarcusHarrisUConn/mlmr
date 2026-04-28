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
Launches the mlmr Shiny interface for HLM-style multilevel modeling with lme4.
}
