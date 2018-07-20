#' Misc functions with ggplot2
#'
#' `gghist` is a simple alternative to `hist()`
#' @param x vector
#' @param ... passed to geom_*()
#' @rdname ggmisc
#' @export
gghist = function(x, ...) {
  if (is.double(x)) {
    geom = ggplot2::geom_histogram
  } else {
    geom = ggplot2::geom_bar
  }
  ggplot2::ggplot(tibble::tibble(x = x), ggplot2::aes(x)) + geom(...)
}

#' `ggsave_quartz` is a shortcut of `ggplot2::ggsave()` with quartz device
#' @inheritParams ggplot2::ggsave
#' @rdname ggmisc
#' @export
ggsave_quartz = function(
                         filename, plot=ggplot2::last_plot(), device=grDevices::quartz,
                         path=NULL, scale=1, width=7, height=7,
                         units=c("in", "cm", "mm"), dpi=300, limitsize=TRUE) {
  ext = tools::file_ext(filename)
  stopifnot(ext %in% c("png", "pdf"))
  ggplot2::ggsave(
    filename, plot, device, path,
    scale, width, height, units, dpi, limitsize, type = ext
  )
}

#' `mean_sd` calculates mean and sd for `ggplot2::stat_summary()`
#' @inheritParams ggplot2::mean_se
#' @rdname graphics
#' @export
mean_sd = function(x, mult = 1.96) {
  x = stats::na.omit(x)
  div = mult * stats::sd(x)
  mu = mean(x)
  data.frame(y = mu, ymin = mu - div, ymax = mu + div)
}
