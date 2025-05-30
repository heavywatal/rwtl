% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parallel.R
\name{mcmap}
\alias{mcmap}
\alias{mcmap_lgl}
\alias{mcmap_int}
\alias{mcmap_dbl}
\alias{mcmap_chr}
\alias{mcmap_vec}
\alias{mcwalk}
\title{Parallel execution in the purrr style}
\usage{
mcmap(.x, .f, ...)

mcmap_lgl(.x, .f, ...)

mcmap_int(.x, .f, ...)

mcmap_dbl(.x, .f, ...)

mcmap_chr(.x, .f, ...)

mcmap_vec(.x, .f, ..., .ptype = NULL)

mcwalk(.x, .f, ...)
}
\arguments{
\item{.x}{A vector or list to be mapped over.}

\item{.f}{A function or formula to be applied to each element of \code{.x}.}

\item{...}{Additional arguments passed to \code{\link[parallel:mclapply]{parallel::mclapply()}}.}

\item{.ptype}{A prototype to specify the output type.}
}
\value{
A list or vector of the same length as \code{.x}.
\code{\link[purrr:list_simplify]{purrr::list_simplify()}} is used to convert the result to a vector.
\code{mcwalk()} returns the input \code{.x} invisibly.
}
\description{
\verb{mcmap*()} are simple wrappers around \code{\link[parallel:mclapply]{parallel::mclapply()}} that allow the
usage similar to the \code{\link[purrr:map]{purrr::map()}} family.
}
\seealso{
\url{https://furrr.futureverse.org/}
}
