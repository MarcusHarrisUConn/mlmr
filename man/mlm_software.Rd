\name{mlm_software_table}
\alias{mlm_software_table}
\alias{mlm_software_apa}
\alias{mlm_papaja_code}
\title{Report R and Package Versions}
\usage{
mlm_software_table(packages = mlm_default_packages())

mlm_software_apa(packages = mlm_default_packages())

mlm_papaja_code(packages = mlm_default_packages(), bib_file = "r-references.bib")
}
\arguments{
\item{packages}{Character vector of software or R package names to report. Use
\code{"R"} to include the R version.}
\item{bib_file}{Name of the BibTeX file to create or update when using the
optional \pkg{papaja} workflow.}
}
\description{
Creates a reproducibility-oriented table and APA-style prose statement reporting
the R version and package versions used in an analysis.
}
\details{
\code{mlm_papaja_code()} returns R code that users can run when \pkg{papaja} is
installed. The generated code uses \code{papaja::r_refs()} to create package
references and \code{papaja::cite_r()} to create an APA-style R/package citation
statement. \pkg{papaja} is optional and is not required to install or run
\pkg{mlmr}.
}
\value{
\code{mlm_software_table()} returns a data frame. \code{mlm_software_apa()}
returns a manuscript-ready prose statement. \code{mlm_papaja_code()} returns
character lines of reproducible R code.
}
\examples{
mlm_software_table()
mlm_software_apa()
cat(mlm_papaja_code(), sep = "\n")
}
