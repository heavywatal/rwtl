#' calculate moment for arbitrary order
#' @param x numeric vector
#' @param order integer
#' @param n length of x
#' @rdname moment
#' @export
moment = function(x, order, n = length(x)) {
  sum(x**order) / n
}

#' @rdname moment
#' @export
skewness = function(x) {
  n = length(x)
  x = x - mean(x)
  m2 = moment(x, 2L, n)
  m3 = moment(x, 3L, n)
  m3 / (m2**1.5)
}

#' kurtosis - 3 is called excess kurtosis, which is 0 for normal distribution
#' @rdname moment
#' @export
kurtosis = function(x) {
  n = length(x)
  x = x - mean(x)
  m2 = moment(x, 2L, n)
  m4 = moment(x, 4L, n)
  m4 / (m2**2)
}

#' @rdname moment
#' @export
bimodality = function(x) {
  n = length(x)
  x = x - mean(x)
  square_sum = sum(x**2L)
  s2 = square_sum / (n - 1)
  m2 = square_sum / n
  m3 = moment(x, 3L, n)
  m4 = moment(x, 4L, n)
  sample_skew = m3 / (s2**1.5)
  excess_kurt = m4 / (m2**2) - 3
  .bimodality(sample_skew, excess_kurt, n)
}

.bimodality = function(sample_skewness, excess_kurtosis, n) {
  (sample_skewness**2 + 1) / (excess_kurtosis + 3 * ((n - 1)**2) / ((n - 2) * (n - 3)))
}
