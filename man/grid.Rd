% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grid.R
\name{gprint}
\alias{gprint}
\alias{grid.draw.list}
\alias{as_gg}
\title{Wrappers of grid functions}
\usage{
gprint(x, newpage = TRUE, recording = FALSE)

\method{grid.draw}{list}(x, recording = FALSE)

as_gg(grob, base_size = 12, base_family = "sans")
}
\arguments{
\item{x}{An object of class \code{"grob"} or NULL.}

\item{newpage}{logical}

\item{recording}{A logical value to indicate whether the drawing
    operation should be recorded on the Grid display list.}

\item{grob}{graphical object}

\item{base_size, base_family}{see ggplot2::ggtheme}
}
\description{
\code{gprint} is a shortcut for grid::grid.draw().
}
\details{
\code{grid.draw.list} enables writing multi-page PDF via ggsave().

\code{as_gg} converts grob to gg.
}
