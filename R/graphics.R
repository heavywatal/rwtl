#' Utilities for graphics
#'
#' @description
#' `pch_plot()` shows pch variations.
#' @param alpha,stroke passed to [ggplot2::geom_point()]
#' @rdname graphics
#' @export
pch_plot = function(alpha = 1, stroke = 1) {
  pch = seq_len(128L) - 1L
  df = data.frame(pch, x = pch %% 16L, y = pch %/% 16L)
  ggplot2::ggplot(df) +
    ggplot2::aes(.data[["x"]], .data[["y"]], label = .data[["pch"]]) +
    ggplot2::geom_point(ggplot2::aes(shape = .data[["pch"]]),
      size = 5, alpha = alpha, stroke = stroke, fill = "tomato"
    ) +
    ggplot2::geom_text(size = 4, position = ggplot2::position_nudge(0, 0.4)) +
    ggplot2::scale_shape_identity() +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_void()
}

#' @description
#' `gghist()` is a simple alternative to [hist()]
#' @param x vector
#' @param ... passed to [ggplot2::geom_histogram()] or [ggplot2::geom_bar()]
#' @inheritParams ggplot2::geom_histogram
#' @rdname graphics
#' @export
gghist = function(x, ..., binwidth = NULL, bins = NULL) {
  geom = if (is.double(x)) {
    ggplot2::geom_histogram(..., binwidth = binwidth, bins = bins)
  } else {
    ggplot2::geom_bar(...)
  }
  ggplot2::ggplot(tibble::tibble(x = x)) +
    ggplot2::aes(x) +
    geom
}

#' @description
#' `col2hex()` translates color name to hex.
#' @param color a character vector
#' @rdname graphics
#' @export
col2hex = function(color = grDevices::colors()) {
  grDevices::rgb(t(grDevices::col2rgb(color)), maxColorValue = 255)
}
