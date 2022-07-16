#' Color palette for ggplot2
#'
#' `scale_color_gradientb()` and `scale_fill_gradientb()` are shortcut of
#' `scale_color_gradientn(colors = scales::brewer_pal(...)(n))` and
#' `scale_fill_gradientn(colors = scales::brewer_pal(...)(n))`, respectively.
#' They are more flexible than [ggplot2::scale_color_distiller()],
#' where the numbers of colors is fixed to 7.
#' @inheritParams scales::brewer_pal
#' @inheritParams ggplot2::scale_color_gradientn
#' @param n Number of colors in the palette.
#' @examples
#' ggplot2::ggplot(mtcars) +
#'   ggplot2::aes(disp, mpg, color = hp, fill = wt) +
#'   ggplot2::geom_point(shape = 22, size = 4, stroke = 2) +
#'   scale_color_gradientb("RdYlGn", direction = -1) +
#'   scale_fill_gradientb("Blues", direction = -1)
#' @rdname palette
#' @export
scale_color_gradientb = function(palette, n = 5L, direction = 1, ...) {
  pal = scales::brewer_pal(palette = palette, direction = direction)(n)
  if (direction < 0) {
    pal = rev(pal)
  }
  ggplot2::scale_color_gradientn(colors = pal, ...)
}

#' @rdname palette
#' @export
scale_fill_gradientb = function(palette, n = 5L, direction = 1, ...) {
  pal = scales::brewer_pal(palette = palette, direction = direction)(n)
  if (direction < 0) {
    pal = rev(pal)
  }
  ggplot2::scale_fill_gradientn(colors = pal, ...)
}
