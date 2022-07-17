#' Helper functions to make labels in ggplot
#'
#' @description
#' `breaks_log10()` makes powers of ten for
#' the `breaks` argument of `ggplot2::scale_*_log10()`.
#' @examples
#' ggplot2::ggplot(tibble::tibble(x = seq_len(10), y = 2**x)) +
#'   ggplot2::geom_point() + ggplot2::aes(x, y) +
#'   ggplot2::scale_y_log10(breaks = breaks_log10, labels = labels_log10)
#' @param limits numeric
#' @rdname gglabel
#' @export
breaks_log10 = function(limits) {
  loglims = log10(limits)
  10**seq(floor(min(loglims)), ceiling(max(loglims)))
}

#' @description
#' `labels_log10()` is an axis label formatter to be passed to
#' the `labels` argument of `ggplot2::scale_*_log10()`.
#' @param breaks numeric
#' @rdname gglabel
#' @export
labels_log10 = function(breaks) {
  parse(text = paste0("10^", log10(breaks)))
}
