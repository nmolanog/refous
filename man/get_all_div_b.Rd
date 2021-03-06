% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/refous.R
\name{get_all_div_b}
\alias{get_all_div_b}
\title{get_all_div_b Function}
\usage{
get_all_div_b(bse, x)
}
\arguments{
\item{bse}{integer from 2 to 9. base of the numbers.}

\item{x}{number in base bse.}
}
\value{
vector with divisors
}
\description{
this functions generates the set of al the divisors of a number in a given base. 1 an the number are not in this set.
}
\examples{
get_all_div_b(5,30)
get_all_div_b(5,20)
}
\keyword{base}
