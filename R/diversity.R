#' Diversity index
#'
#' @param freqs a numeric vector
#' @inheritParams base::log
#' @rdname diversity
#' @export
shannon_index = function(freqs, base = exp(1)) {
  freqs = freqs / sum(freqs)
  -sum(freqs * log(freqs, base))
}

#' @rdname diversity
#' @export
simpson_index = function(freqs) {
  freqs = freqs / sum(freqs)
  sum(freqs**2)
}

#' @description
#' `evenness()` calculates H / H_max
#' @param species a factor vector
#' @rdname diversity
#' @export
evenness = function(species) {
  freqs = table(species)
  shannon_index(freqs) / log(length(freqs))
}
