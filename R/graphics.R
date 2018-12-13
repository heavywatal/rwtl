#' Utilities for graphics
#'
#' `pch_plot` shows pch variations.
#' @rdname graphics
#' @export
pch_plot = function() {
  df = tibble::tibble(
    pch = seq_len(128L) - 1L,
    x = .data$pch %% 16L,
    y = .data$pch %/% 16L
  )
  ggplot2::ggplot(df, ggplot2::aes_(~x, ~y)) +
    ggplot2::geom_text(ggplot2::aes_(label = ~pch), size = 4, position = ggplot2::position_nudge(0, 0.3)) +
    ggplot2::geom_point(ggplot2::aes_(shape = ~pch), size = 5, stroke = 2, fill = "tomato") +
    ggplot2::scale_shape_identity() +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_void()
}

#' @details
#' `gghist` is a simple alternative to `hist()`
#' @param x vector
#' @param ... passed to geom_*()
#' @inheritParams ggplot2::geom_histogram
#' @rdname graphics
#' @export
gghist = function(x, ..., binwidth = NULL, bins = NULL) {
  p = ggplot2::ggplot(tibble::tibble(x = x), ggplot2::aes(x))
  if (is.double(x)) {
    p + ggplot2::geom_histogram(..., binwidth = binwidth, bins = bins)
  } else {
    p + ggplot2::geom_bar(...)
  }
}

#' @details
#' `col2hex` translates color name to hex.
#' @param color a character vector
#' @rdname graphics
#' @export
col2hex = function(color = grDevices::colors()) {
  grDevices::rgb(t(grDevices::col2rgb(color)), max = 255)
}
