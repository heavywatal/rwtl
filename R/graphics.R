#' Utilities for graphics
#'
#' `pch_plot` shows pch variations.
#' @rdname graphics
#' @export
pch_plot = function() {
  col = "red"
  bg = "orange"
  graphics::plot(
    0, 0, xlim = c(0, 6), ylim = c(0, 4),
    type = "n", axes = FALSE, xlab = "", ylab = ""
  )
  a = rep(0:5, rep(5, 6))
  b = c(4:0, 4:0, 4:0, 4:0, 4:0, 4:0)
  graphics::points(a + 0.3, b, pch = 0:29, cex = 3, col = col, bg = bg)
  graphics::text(a, b, 0:29)
  graphics::mtext(paste0("col='", col, "', bg='", bg, "'"))
}

#' @description
#' `col2hex` translates color name to hex.
#' @param color a character vector
#' @rdname graphics
#' @export
col2hex = function(color=grDevices::colors()) {
  grDevices::rgb(t(grDevices::col2rgb(color)), max = 255)
}
