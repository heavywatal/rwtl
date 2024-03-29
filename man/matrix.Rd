% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix.R
\name{printmat}
\alias{printmat}
\title{Utilities for matrix}
\usage{
printmat(
  x,
  fmt = "",
  upper = TRUE,
  lower = TRUE,
  diag = TRUE,
  digits = getOption("digits"),
  right = TRUE,
  ...
)
}
\arguments{
\item{x}{a numeric matrix.}

\item{fmt}{a character vector of format strings, each of up to 8192 bytes.}

\item{upper, lower, diag}{a logical value.}

\item{digits}{minimal number of \emph{significant} digits, see
    \code{\link[base]{print.default}}.}

\item{right}{logical, indicating whether or not strings should be
    right aligned.}

\item{...}{passed to \code{print()}.}
}
\description{
\code{printmat()} supports triangular printing of a matrix.
}
\seealso{
\code{\link[=printdf]{printdf()}}
}
