#' Utilities for matrix
#'
#' @details
#' `printmat()` supports triangular printing of a matrix.
#' @param x a numeric matrix.
#' @param upper,lower,diag a logical value.
#' @param ... passed to `print()`.
#' @inheritParams base::sprintf
#' @inheritParams base::print
#' @seealso [printdf()]
#' @rdname matrix
#' @export
printmat = function(x, fmt = "", upper = TRUE, lower = TRUE, diag = TRUE,
                    digits = getOption("digits"), right = TRUE, ...) {
  if (nzchar(fmt)) {
    m = sprintf(fmt, x) |>
      matrix(nrow = nrow(x), dimnames = dimnames(x))
  } else {
    m = format(x, trim = TRUE, digits = digits, justify = "none")
  }
  if (!upper) {
    m[upper.tri(m, diag = !diag)] = ""
  }
  if (!lower) {
    m[lower.tri(m, diag = !diag)] = ""
  }
  print(m, quote = FALSE, right = right, justify = "none", ...)
  invisible(x)
}
