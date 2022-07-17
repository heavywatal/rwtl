#' Variations of `ggplot2::annotate()`
#'
#' @description
#' `annotate_polygon()` is a shortcut of `ggplot2::annotate("polygon")`
#' to draw a regular polygon.
#' @param x,y center coordinate
#' @param n number of sides
#' @param radius size of polygon
#' @param stroke line width
#' @param ... passed to `ggplot2::annotate("polygon")`
#' @examples
#' ggplot2::ggplot() +
#'   annotate_polygon2(0, 0, n = 6L, radius = 5, fill = "#ffffff", color = "#333333", stroke = 6) +
#'   annotate_polygon(0, 0, n = 6L, radius = 2, fill = "#C41A41")
#' @rdname ggannotate
#' @export
annotate_polygon = function(x, y, n = 6L, radius = 1, stroke = 1, ...) {
  angle = seq_len(n) * 2 * pi / n + pi / 2
  x = x + radius * cos(angle)
  y = y + radius * sin(angle)
  ggplot2::annotate("polygon", x = x, y = y, size = stroke, ...)
}

#' @description
#' `annotate_polygon2()` has more control on line stroke.
#' @param color,linejoin passed to `ggplot2::annotate("path")`
#' @rdname ggannotate
#' @export
annotate_polygon2 = function(x, y, n = 6L, radius = 1, stroke = 1, color = "#666666", linejoin = "mitre", ...) {
  angle = seq_len(n + 2L) * 2 * pi / n + pi / 2
  x = x + radius * cos(angle)
  y = y + radius * sin(angle)
  list(
    ggplot2::annotate("polygon", x = x, y = y, ...),
    ggplot2::annotate("path", x = x, y = y, size = stroke, color = color, linejoin = linejoin)
  )
}
