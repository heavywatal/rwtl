#' Utilities for vector
#'
#' @details
#' [twist()] cycles a vector
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

#' @details
#' [split_consecutive()] groups consecutive integers.
#' @rdname vector
#' @export
split_consecutive = function(x) {
  boundary = (diff(x) != 1L)
  group = c(0L, cumsum(boundary))
  unname(split(x, group))
}
