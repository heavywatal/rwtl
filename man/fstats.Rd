% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fstats.R
\name{fst_HBK}
\alias{fst_HBK}
\alias{fst_HSM}
\title{Fst estimators}
\usage{
fst_HBK(within, between, n = 2)

fst_HSM(within, between)
}
\arguments{
\item{within, between}{mean branch length or diversity}

\item{n}{number of subpopulations}
}
\description{
\code{fst_HBK()} calculates Kst by Hudson, Boos, and Kaplan (1992).

\code{fst_HSM()} calculates Fst by Hudson, Slatkin, and Maddison (1992).
}
