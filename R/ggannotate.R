#' Variations of `ggplot2::annotate()`
#'
#' @description
#' `annotate_polygon()` is a shortcut of `ggplot2::annotate("polygon")`
#' @param x,y center coordinate
#' @param radius size of polygon
#' @param stroke line width
#' @param n number of sides
#' @param ... passed to `ggplot2::annotate()`
#' @rdname ggannotate
#' @export
annotate_polygon = function(x, y, radius = 1, stroke = 1, n = 6L, ...) {
  angle = seq_len(n + 1L) * 2 * pi / n + pi / 2
  x = x + radius * cos(angle)
  y = y + radius * sin(angle)
  ggplot2::annotate("polygon", x = x, y = y, size = stroke, ...)
}
