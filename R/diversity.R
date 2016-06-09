#' Shannon index
#' @param freqs a numeric vector
#' @param base a numeric
#' @return a numeric
#' @rdname diversity
#' @export
shannon_index = function(freqs, base=exp(1)) {
    freqs = freqs[freqs > 0] / sum(freqs)
    -sum(freqs * log(freqs, base))
}

#' Simpson index
#' @return a numeric
#' @rdname diversity
#' @export
simpson_index = function(freqs) {
    freqs = freqs[freqs > 0]
    sum(freqs ^ 2) / (sum(freqs) ^ 2)
}
