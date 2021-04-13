#' Wrappers of grid functions
#'
#' `gprint` is a shortcut for grid::grid.draw().
#' @param newpage logical
#' @inheritParams grid::grid.draw
#' @rdname grid
#' @export
gprint = function(x, newpage = TRUE, recording = FALSE) {
  if (newpage) grid::grid.newpage(recording)
  grid::grid.draw(x, recording)
  invisible(x)
}

#' @details
#' `grid.draw.list` enables writing multi-page PDF via ggsave().
#' @inheritParams grid::grid.draw
#' @importFrom grid grid.draw
#' @method grid.draw list
#' @rdname grid
#' @export
grid.draw.list = function(x, recording = FALSE) {
  lapply(x, grid::grid.draw, recording = recording)
  invisible(x)
}

#' @details
#' `png_grob` creates a grob from a PNG file.
#' @param file path to PNG file
#' @rdname grid
#' @export
png_grob = function(file) {
  grid::rasterGrob(png::readPNG(file))
}

#' @details
#' `as_gg` converts grob to gg.
#' @param grob graphical object
#' @param base_size,base_family see ggplot2::ggtheme
#' @rdname grid
#' @export
as_gg = function(grob, base_size = 12, base_family = "sans") {
  ggplot2::ggplot(data.frame(x = 0:1, y = 0:1)) +
    ggplot2::annotation_custom(grob) +
    ggplot2::scale_x_continuous(limits = 0:1, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = 0:1, expand = c(0, 0)) +
    ggplot2::theme_void(base_size, base_family)
}
