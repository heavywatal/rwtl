#' Misc functions with ggplot2
#'
#' @details
#' `insert_layer` insert layers to an arbitrary position
#' @param p gg object
#' @param ... layers to add
#' @param after position; 0L is back
#' @rdname ggmisc
#' @export
insert_layer = function(p, ..., after = 0L) {
  if (after < 0L) {
    after = length(p$layers) + after
  }
  p$layers = append(p$layers, ..., after = after)
  p
}

#' @details
#' `ggsave_quartz` is a shortcut of `ggplot2::ggsave()` with quartz device.
#' @inheritParams ggplot2::ggsave
#' @rdname ggmisc
#' @export
ggsave_quartz = function(filename, plot = ggplot2::last_plot(), device = grDevices::quartz,
                         path = NULL, scale = 1, width = 7, height = 7,
                         units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE) {
  ext = tools::file_ext(filename)
  stopifnot(ext %in% c("png", "pdf"))
  ggplot2::ggsave(
    filename, plot, device, path,
    scale, width, height, units, dpi, limitsize,
    type = ext
  )
}

#' @details
#' `mean_sd` calculates mean and sd for `ggplot2::stat_summary()`.
#' @inheritParams ggplot2::mean_se
#' @rdname ggmisc
#' @export
mean_sd = function(x, mult = 1.96) {
  x = stats::na.omit(x)
  div = mult * stats::sd(x)
  mu = mean(x)
  data.frame(y = mu, ymin = mu - div, ymax = mu + div)
}

#' @details
#' `label_none` removes `strip.text` while keeping `strip.background`.
#' @inheritParams ggplot2::label_value
#' @rdname ggmisc
#' @export
label_none = function(labels, ...) {
  lapply(labels, function(x) character(length(x)))
}
