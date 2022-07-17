#' Variations of `ggplot2::annotate()`
#'
#' @description
#' `annotate_regpolygon()` is a shortcut of `ggplot2::annotate("polygon")`
#' to draw a regular polygon.
#' @param n number of sides
#' @param radius size of polygon
#' @param x,y center coordinate
#' @param start angle of the starting point
#' @param linewidth,color passed to `ggplot2::annotate("path")`
#' @param fill,... passed to `ggplot2::annotate("polygon")`
#' @examples
#' ggplot2::ggplot() +
#'   annotate_regpolygon(6L, radius = 5, linewidth = 6, color = "#333333", fill = "#ffffff") +
#'   annotate_regpolygon(6L, radius = 2, fill = "#C41A41") +
#'   ggplot2::coord_fixed()
#' @rdname ggannotate
#' @export
annotate_regpolygon = function(n, radius = 1, x = 0, y = 0, start = pi / 2,
                               linewidth = 0, color = "#333333", fill = "#666666", ...) {
  df = make_regpolygon(n, radius, x, y, start)
  x = df[["x"]]
  y = df[["y"]]
  layers = list(
    ggplot2::annotate("polygon", x = x, y = y, fill = fill, ...)
  )
  # linetype = "solid" can draw lines but linejoin is fixed to "round"
  if (linewidth > 0) {
    layers[[2]] = ggplot2::annotate("path",
      x = x, y = y, size = linewidth, color = color, linejoin = "mitre"
    )
  }
  layers
}

make_regpolygon = function(n, radius = 1, x = 0, y = 0, start = pi / 2) {
  angle = (seq_len(n + 2L) - 1L) * 2 * pi / n + start
  tibble::tibble(
    x = x + radius * cos(angle),
    y = y + radius * sin(angle)
  )
}
