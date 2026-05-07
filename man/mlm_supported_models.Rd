\name{mlm_supported_models}
\alias{mlm_supported_models}
\title{Summarize the Current mlmr Modeling Scope}
\usage{
mlm_supported_models()
}
\description{
Returns a table describing the modeling, reporting, import, diagnostic, and
export features that are currently supported, experimental, or planned in
\pkg{mlmr}.
}
\details{
This helper is intended for transparent documentation and teaching. It gives
users a compact way to see the current production path, advanced workflows, and
areas that require extra statistical judgment or future development.
}
\value{
A data frame with columns \code{Area}, \code{Status}, \code{Scope}, and
\code{User_responsibility}.
}
\examples{
mlm_supported_models()
}
