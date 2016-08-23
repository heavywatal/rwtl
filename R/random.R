#' Generate random integers between [a, b]
#' @param n integer
#' @param a integer
#' @param b integer
#' @return n integer vector
#' @rdname random
#' @export
#' @examples table(randint(600L, 1L, 6L))
randint = function(n, a, b) {
    1L - a + sample.int(b, n, replace=TRUE)
}
