\name{example_hsb}
\alias{example_hsb}
\alias{example_longitudinal}
\title{Generate Built-In Multilevel and Longitudinal Example Datasets}
\usage{
example_hsb(n_schools = 24, min_students = 18, max_students = 34,
  seed = 20260423)

example_longitudinal(n_students = 80, n_waves = 5, seed = 20260423)
}
\arguments{
\item{n_schools}{Number of schools to simulate in \code{example_hsb()}.}
\item{min_students, max_students}{Minimum and maximum number of students per
school in \code{example_hsb()}.}
\item{n_students}{Number of individuals to simulate in
\code{example_longitudinal()}.}
\item{n_waves}{Number of repeated measurement occasions per individual in
\code{example_longitudinal()}.}
\item{seed}{Random seed used to make the generated data reproducible. The
caller's random-number state is restored afterwards, so calling these helpers
does not alter subsequent random draws. Use \code{NULL} to draw from the
current random-number stream instead.}
}
\description{
Generates built-in multilevel educational (\code{example_hsb()}) and repeated-
measures growth (\code{example_longitudinal()}) datasets used by the \pkg{mlmr}
Shiny app, vignettes, and validation benchmarks.
}
\value{
\code{example_hsb()} returns a data frame containing simulated student, school,
district, and teacher variables (including continuous, binary, and count
outcomes). \code{example_longitudinal()} returns a data frame containing
repeated measurement waves nested within students and schools.
}
\examples{
dat <- example_hsb(n_schools = 4, min_students = 5, max_students = 6)
str(dat)

dat_long <- example_longitudinal(n_students = 10, n_waves = 4)
str(dat_long)
}
