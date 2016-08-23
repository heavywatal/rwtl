#' Shortcut for grid::viewport()
#' @param n integer
#' @param a integer
#' @param b integer
#' @return n integers between [a, b]
#' @rdname random
#' @export
randint = function(n, a, b) {
    as.integer(stats::runif(n, a, b + 1))
}
