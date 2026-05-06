\name{example_hsb}
\alias{example_hsb}
\title{Generate an HSB-Style Multilevel Example Dataset}
\usage{
example_hsb(n_schools = 24, min_students = 18, max_students = 34,
  seed = 20260423)
}
\arguments{
\item{n_schools}{Number of schools to simulate.}
\item{min_students, max_students}{Minimum and maximum number of students per school.}
\item{seed}{Random seed used to make the generated data reproducible.}
}
\description{
Generates the built-in HSB-style example dataset used by the \pkg{mlmr}
Shiny app. The data include student-level predictors, school-level predictors,
categorical predictors, grouping factors, and outcomes suitable for mixed-effects
model demonstrations.
}
\value{
A data frame containing simulated student, school, district, and teacher
variables.
}
\examples{
dat <- example_hsb(n_schools = 4, min_students = 5, max_students = 6)
str(dat)
}
