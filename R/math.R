#' Permutation
#' @param n, k numeric vectors
#' @return numeric
#' @rdname math
#' @export
permutate = function(n, k) choose(n, k) * factorial(k)

#' Probit function
#' @param p a numeric vector [0, 1]
#' @return a numeric vector
#' @rdname math
#' @export
probit = function(p) {stats::qnorm(p)}

#' Logit function: log-odds; the inverse of sigmoid
#' @param base a numeric
#' @return a numeric vector
#' @rdname math
#' @export
logit = function(p, base=exp(1)) {log(p / (1 - p), base)}

#' Sigmoid function: the inverse of logit
#' @param x a numeric vector
#' @param gain a numeric
#' @return a numeric vector
#' @rdname math
#' @export
sigmoid = function(x, gain=1) {1 / (1 + exp(-gain * x))}

#' Logistic function
#' @param r a numeric vector
#' @param k a numeric vector
#' @param x0 a numeric vector
#' @return a numeric vector
#' @rdname math
#' @export
logistic = function(x, r=1, k=1, x0=0) {k * sigmoid((x - x0), r * k)}

#' Heaviside step function
#' @param h0 return value if x==0
#' @return a numeric vector
#' @rdname math
#' @export
heaviside = function(x, h0=NA) {ifelse(x==0, h0, (x > 0) + 0)}

#' exponent
#' @rdname math
#' @export
exponent = function(x, base=exp(1)) {
    floor(log(abs(x), base))
}

#' mantissa, significand, coefficient
#' @rdname math
#' @export
mantissa = function(x, base=exp(1)) {
    x / base ^ exponent(x, base)
}

#' Split number into mantissa and exponent
#' @rdname math
#' @export
scientific_notation = function(x, base=10) {
    exponent = floor(log(abs(x), base))
    mantissa = x / base ^ exponent
    stats::setNames(c(mantissa, exponent), c("mantissa", "exponent"))
}
