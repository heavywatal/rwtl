#' Utilities for vector
#'
#' @description
#' `twist()` cycles a vector.
#' @param x a vector
#' @param n an integer
#' @rdname vector
#' @export
#' @seealso [dplyr::lead()], [dplyr::lag()]
twist = function(x, n) {
  len = length(x)
  if (n > 0L) {
    c(x[seq_len(len - n) + n], x[seq_len(n)])
  } else {
    c(x[seq_len(-n) + (len + n)], x[seq(len + n)])
  }
}

#' @description
#' `sort_numeric()` sorts strings numerically.
#' @param width the number of digits for zero-padding.
#' @rdname vector
#' @export
sort_numeric = function(x, width = 3L) {
  if (is.numeric(x)) {
    sort(x)
  } else {
    xn = suppressWarnings(readr::parse_number(as.character(x))) |>
      stringr::str_pad(width = width, pad = "0")
    x[order(xn, x)]
  }
}

#' @description
#' `as_factor_numeric()` is a variant of `as.factor()` to sort levels numerically.
#' @param ordered a logical flag passed to `factor()`.
#' @rdname vector
#' @export
as_factor_numeric = function(x, width = 3L, ordered = is.ordered(x)) {
  if (is.factor(x)) {
    levels = levels(x)
  } else {
    levels = unique(x)
  }
  factor(x, levels = sort_numeric(levels, width = width), ordered = ordered)
}

#' @description
#' `split_consecutive()` groups consecutive integers.
#' @rdname vector
#' @export
split_consecutive = function(x) {
  boundary = (diff(x) != 1L)
  group = c(0L, cumsum(boundary))
  unname(split(x, group))
}
