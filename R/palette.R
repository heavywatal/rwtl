#' Colour palette for ggplot2
#'
#' @inheritParams scales::brewer_pal
#' @inheritParams ggplot2::scale_colour_gradientn
#' @param n Number of colors in the palette.
#' @examples
#' ggplot2::ggplot(mtcars) +
#'   ggplot2::aes(disp, mpg, color = hp, fill = wt) +
#'   ggplot2::geom_point(chape = 22, size = 4, stroke = 2) +
#'   scale_colour_gradientb("RdYlGn", direction = -1) +
#'   scale_fill_gradientb("Blues", direction = -1)
#' @details `scale_colour_gradientb` is a shortcut of
#'   `scale_colour_gradientn(colours = scales::brewer_pal(...))`
#' @rdname palette
#' @export
scale_colour_gradientb = function(palette, n = 5L, direction = 1, ...) {
  pal = scales::brewer_pal(palette = palette, direction = direction)(n)
  if (direction < 0) {
    pal = rev(pal)
  }
  ggplot2::scale_colour_gradientn(colours = pal, ...)
}

#' @details `scale_fill_gradientb` is a shortcut of
#'   `scale_fill_gradientn(colours = scales::brewer_pal(...))`
#' @rdname palette
#' @export
scale_fill_gradientb = function(palette, n = 5L, direction = 1, ...) {
  pal = scales::brewer_pal(palette = palette, direction = direction)(n)
  if (direction < 0) {
    pal = rev(pal)
  }
  ggplot2::scale_fill_gradientn(colours = pal, ...)
}
