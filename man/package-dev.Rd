% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.R
\name{document_install}
\alias{document_install}
\alias{chk}
\alias{tst}
\title{Package development utilities}
\usage{
document_install(pkg = ".", build = FALSE, upgrade = FALSE, ...)

chk(pkg = ".", install = FALSE, vignettes = FALSE, args = "--timings", ...)

tst(pkg = ".", ...)
}
\arguments{
\item{pkg}{The package to use, can be a file path to the package or a
package object.  See \code{\link[devtools:as.package]{as.package()}} for more information.}

\item{build}{if \code{TRUE} \code{\link[pkgbuild:build]{pkgbuild::build()}}s the package first:
this ensures that the installation is completely clean, and prevents any
binary artefacts (like \file{.o}, \code{.so}) from appearing in your local
package directory, but is considerably slower, because every compile has
to start from scratch.}

\item{upgrade}{Should package dependencies be upgraded? One of "default", "ask", "always", or "never". "default"
respects the value of the \code{R_REMOTES_UPGRADE} environment variable if set,
and falls back to "ask" if unset. "ask" prompts the user for which out of
date packages to upgrade. For non-interactive sessions "ask" is equivalent
to "always". \code{TRUE} and \code{FALSE} are also accepted and correspond to
"always" and "never" respectively.}

\item{...}{additional arguments passed to \code{\link[remotes:install_deps]{remotes::install_deps()}}
when installing dependencies.}

\item{install}{If FALSE, add \code{--no-install} to \code{args}}

\item{vignettes}{If FALSE, add \code{--ignore-vignettes} to \code{args}}

\item{args}{An optional character vector of additional command line
arguments to be passed to \verb{R CMD INSTALL}. This defaults to the
value of the option \code{"devtools.install.args"}.}
}
\description{
\code{document_install()} is a quick shortcut of \code{\link[devtools:install]{devtools::install()}}
following \code{\link[devtools:document]{devtools::document()}}.

\code{chk()} and \code{tst()} are thin wrappers of \code{\link[devtools:check]{devtools::check()}} and \code{\link[testthat:test_package]{testthat::test_local()}}.
}
