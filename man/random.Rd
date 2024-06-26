% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random.R
\name{runif.int}
\alias{runif.int}
\title{Generate random integers between \code{min} and \code{max}.}
\usage{
runif.int(n, min = -.Machine$integer.max, max = .Machine$integer.max)
}
\arguments{
\item{n}{number of observations}

\item{min, max}{lower and upper limits of the distribution}
}
\description{
Generate random integers between \code{min} and \code{max}.
}
\examples{
table(runif.int(600L, 1L, 6L))
}
