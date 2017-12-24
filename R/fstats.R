#' Calculate Kst by Hudson, Boos, and Kaplan (1992)
#' @param within,between mean branch length or diversity
#' @param n number of subpopulations
#' @return numeric
#' @rdname fstats
#' @export
fst_HBK = function(within, between, n=2) {
  (between - within) / (between + within / (n - 1))
}

#' Calculate Fst by Hudson, Slatkin, and Maddison (1992)
#' @return numeric
#' @rdname fstats
#' @export
fst_HSM = function(within, between) {
  1.0 - within / between
}
