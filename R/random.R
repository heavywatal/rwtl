#' Generate random integers between [a, b]
#' @param n number of observations
#' @param min,max lower and upper limits of the distribution
#' @return n integer vector
#' @rdname random
#' @export
#' @examples table(runif.int(600L, 1L, 6L))
runif.int = function(n, min, max) {
    min_1 = min - 1L
    sample.int(max - min_1, n, replace=TRUE) + min_1
}