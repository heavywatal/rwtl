#' Utilities for graphics
#'
#' `pch_plot` shows pch variations.
#' @rdname graphics
#' @export
pch_plot = function() {
  df = tibble::tibble(
    pch = seq_len(128L) - 1L,
    x = pch %% 16L,
    y = pch %/% 16L
  )
  ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_text(ggplot2::aes(label = pch), size = 4, position = ggplot2::position_nudge(0, 0.3)) +
    ggplot2::geom_point(ggplot2::aes(shape = pch), size = 5, stroke = 2, fill = "tomato") +
    ggplot2::scale_shape_identity() +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_void()
}

#' @description
#' `col2hex` translates color name to hex.
#' @param color a character vector
#' @rdname graphics
#' @export
col2hex = function(color = grDevices::colors()) {
  grDevices::rgb(t(grDevices::col2rgb(color)), max = 255)
}
