#' Utilities for data.frame
#'
#' @param .data A data.frame.
#' @inheritParams dplyr::across
#' @rdname dataframe
#' @export
center_range = function(.data, .cols) {
  center = function(x) x - mean(range(x))
  dplyr::mutate(.data, dplyr::across({{ .cols }}, center))
}

#' @rdname dataframe
#' @export
class_at = function(.data, .cols) {
  x = dplyr::summarize(.data, dplyr::across({{ .cols }}, class))
  unlist(x, use.names = TRUE)
}

#' Shortcut for [tidyr::crossing()] with repeats.
#' @param x atomic vector
#' @param times the number of repeats
#' @return tbl
#' @rdname itertools
#' @export
crossing_rep = function(x, times = 1L) {
  x = rep.int(list(x), times)
  x = stats::setNames(x, paste0("v", seq_len(times)))
  rlang::exec(tidyr::crossing, !!!x)
}
