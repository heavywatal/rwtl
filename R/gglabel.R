#' Helper functions to make labels in ggplot
#'
#' `breaks_log10` makes powers of ten for
#' the `breaks` argument of `ggplot2::scale_*_log10()`.
#' @param limits numeric
#' @rdname gglabel
#' @export
breaks_log10 = function(limits) {
  loglims = log10(limits)
  10**seq(floor(min(loglims)), ceiling(max(loglims)))
}

#' @description
#' `labels_log10` is an axis label formatter to be passed to
#' the `labels` argument of `ggplot2::scale_*_log10()`.
#' @param breaks numeric
#' @rdname gglabel
#' @export
labels_log10 = function(breaks) {
  parse(text = paste0("10^", log10(breaks)))
}
