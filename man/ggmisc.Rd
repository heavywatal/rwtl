% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggmisc.R
\name{insert_layer}
\alias{insert_layer}
\alias{ggsave_quartz}
\alias{mean_sd}
\alias{label_none}
\title{Misc functions with ggplot2}
\usage{
insert_layer(p, ..., after = 0L)

ggsave_quartz(
  filename,
  plot = ggplot2::last_plot(),
  device = grDevices::quartz,
  path = NULL,
  scale = 1,
  width = 7,
  height = 7,
  units = c("in", "cm", "mm"),
  dpi = 300,
  limitsize = TRUE
)

mean_sd(x, mult = 1.96)

label_none(labels, ...)
}
\arguments{
\item{p}{gg object}

\item{...}{layers to add}

\item{after}{position; 0L is back}

\item{filename}{File name to create on disk.}

\item{plot}{Plot to save, defaults to last plot displayed.}

\item{device}{Device to use. Can either be a device function
(e.g. \link{png}), or one of "eps", "ps", "tex" (pictex),
"pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).}

\item{path}{Path of the directory to save plot to: \code{path} and \code{filename}
are combined to create the fully qualified file name. Defaults to the
working directory.}

\item{scale}{Multiplicative scaling factor.}

\item{width}{Plot size in \code{units} ("in", "cm", "mm", or "px").
If not supplied, uses the size of current graphics device.}

\item{height}{Plot size in \code{units} ("in", "cm", "mm", or "px").
If not supplied, uses the size of current graphics device.}

\item{units}{Plot size in \code{units} ("in", "cm", "mm", or "px").
If not supplied, uses the size of current graphics device.}

\item{dpi}{Plot resolution. Also accepts a string input: "retina" (320),
"print" (300), or "screen" (72). Applies only to raster output types.}

\item{limitsize}{When \code{TRUE} (the default), \code{ggsave()} will not
save images larger than 50x50 inches, to prevent the common error of
specifying dimensions in pixels.}

\item{x}{numeric vector.}

\item{mult}{number of multiples of standard error.}

\item{labels}{Data frame of labels. Usually contains only one
element, but faceting over multiple factors entails multiple
label variables.}
}
\description{
Misc functions with ggplot2
}
\details{
\code{insert_layer} insert layers to an arbitrary position

\code{ggsave_quartz} is a shortcut of \code{ggplot2::ggsave()} with quartz device.

\code{mean_sd} calculates mean and sd for \code{ggplot2::stat_summary()}.

\code{label_none} removes \code{strip.text} while keeping \code{strip.background}.
}
