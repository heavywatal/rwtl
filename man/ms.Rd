% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ms.R
\name{run_ms}
\alias{run_ms}
\alias{parse_ms}
\alias{split_ms}
\alias{as_int_matrix}
\title{Functions to run ms and parse ms-like output}
\usage{
run_ms(nsam = 4L, nreps = 2L, theta = 5)

parse_ms(msout, byrow = FALSE)

split_ms(msout)

as_int_matrix(samples, byrow = FALSE)
}
\arguments{
\item{nsam}{number of samples}

\item{nreps}{number of repeats}

\item{theta}{population mutation rate}

\item{msout}{string vector of ms-like output}

\item{byrow}{logical value passed to \code{matrix}}

\item{samples}{string vector}
}
\value{
\code{run_ms()} returns a string vector

\code{parse_ms()} returns a list of integer matrices

\code{split_ms()} returns a list of string vector

\code{as_int_matrix()} returns an integer matrix
}
\description{
\code{run_ms()} executes ms as an external program.

\code{parse_ms()} transforms ms-like output into a list of matrices.

\code{split_ms()} splits ms-like output by replications.

\code{as_int_matrix()} converts a string vector into an integer matrix.
}
