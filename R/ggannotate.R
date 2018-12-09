#' Variations of `ggplot2::annotate()`
#'
#' @description
#' `annotate_polygon()` is a shortcut of `ggplot2::annotate("polygon")`.
#' @param x,y center coordinate
#' @param n number of sides
#' @param radius size of polygon
#' @param stroke line width
#' @param ... passed to `ggplot2::annotate("polygon")`
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
#' @param colour,linejoin passed to `ggplot2::annotate("path")`
#' @rdname ggannotate
#' @export
annotate_polygon2 = function(x, y, n = 6L, radius = 1, stroke = 1, colour = "#666666", linejoin = "mitre", ...) {
  angle = seq_len(n + 2L) * 2 * pi / n + pi / 2
  x = x + radius * cos(angle)
  y = y + radius * sin(angle)
  list(
    ggplot2::annotate("polygon", x = x, y = y, ...),
    ggplot2::annotate("path", x = x, y = y, size = stroke, colour = colour, linejoin = linejoin)
  )
}
