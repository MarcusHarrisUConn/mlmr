\name{mlm_software_table}
\alias{mlm_software_table}
\alias{mlm_software_apa}
\title{Report R and Package Versions}
\usage{
mlm_software_table(packages = mlm_default_packages())

mlm_software_apa(packages = mlm_default_packages())
}
\arguments{
\item{packages}{Character vector of software or R package names to report. Use
\code{"R"} to include the R version.}
}
\description{
Creates a reproducibility-oriented table and APA-style prose statement reporting
the R version and package versions used in an analysis.
}
\value{
\code{mlm_software_table()} returns a data frame. \code{mlm_software_apa()}
returns a manuscript-ready prose statement.
}
\examples{
mlm_software_table()
mlm_software_apa()
}
