#' Variations of `ggplot2::annotate()`
#'
#' @description
#' `annotate_regpolygon()` is a shortcut of `ggplot2::annotate("polygon")`
#' to draw a regular polygon.
#' @param n The number of sides.
#' @param radius The distance from the center to a vertex.
#' @param x,y The center coordinates.
#' @param start The angle of the starting point.
#' @param linejoin,... Extra arguments passed to `ggplot2::annotate("polygon")`.
#' @examples
#' ggplot2::ggplot() +
#'   annotate_regpolygon(6L, radius = 5, linewidth = 6, color = "#333333", fill = "#ffffff") +
#'   annotate_regpolygon(6L, radius = 2, fill = "#C41A41", linetype = 0, linewidth = 0) +
#'   ggplot2::coord_fixed()
#' @rdname ggannotate
#' @export
annotate_regpolygon = function(n, radius = 1, x = 0, y = 0, start = pi / 2, linejoin = "mitre", ...) {
  .df = make_regpolygon(n, radius, x, y, start)
  ggplot2::annotate("polygon",
    x = .df[["x"]], y = .df[["y"]], linejoin = linejoin, ...
  )
}

make_regpolygon = function(n, radius = 1, x = 0, y = 0, start = pi / 2) {
  angle = (seq_len(n) - 1L) * 2 * pi / n + start
  tibble::new_tibble(list(
    x = x + radius * cos(angle),
    y = y + radius * sin(angle)
  ))
}
