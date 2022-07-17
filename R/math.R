#' Shortcut to mathematical functions
#'
#' @param n,k numeric vectors
#' @rdname math
#' @export
permutate = function(n, k) choose(n, k) * factorial(k)

#' @param p a numeric vector between 0 and 1
#' @rdname math
#' @export
probit = function(p) {
  stats::qnorm(p)
}

# inverse of sigmoid
#' @param base a numeric
#' @rdname math
#' @export
logit = function(p, base = exp(1)) {
  log(p / (1 - p), base)
}

# inverse of logit
#' @param x a numeric vector
#' @param gain a numeric
#' @rdname math
#' @export
sigmoid = function(x, gain = 1) {
  1 / (1 + exp(-gain * x))
}

#' @param r a numeric vector
#' @param x0 a numeric vector
#' @rdname math
#' @export
logistic = function(x, r = 1, k = 1, x0 = 0) {
  k * sigmoid((x - x0), r * k)
}

#' @rdname math
#' @export
exponent = function(x, base = exp(1)) {
  floor(log(abs(x), base))
}

#' @rdname math
#' @export
mantissa = function(x, base = exp(1)) {
  x / base**exponent(x, base)
}

#' @description
#' `scientific_notation()` splits number into mantissa and exponent.
#' @rdname math
#' @export
scientific_notation = function(x, base = 10) {
  exponent = floor(log(abs(x), base))
  mantissa = x / base**exponent
  stats::setNames(c(mantissa, exponent), c("mantissa", "exponent"))
}
