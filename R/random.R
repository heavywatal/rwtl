#' Generate random integers between `min` and `max`.
#' @param n number of observations
#' @param min,max lower and upper limits of the distribution
#' @rdname random
#' @export
#' @examples
#' table(runif.int(600L, 1L, 6L))
runif.int = function(n, min = -.Machine$integer.max, max = .Machine$integer.max) {
  .offset = min - 1.0
  as.integer(sample.int(max - .offset, n, replace = TRUE) + .offset)
}
