#' Utilities for data.frame
#'
#' `center_range()` mutates the selected columns for zero-centered plot.
#' @param .data A data.frame.
#' @param .cols tidy-select passed to [dplyr::across()]
#' @rdname dataframe
#' @export
center_range = function(.data, .cols) {
  center = \(x) x - mean(range(x))
  dplyr::mutate(.data, dplyr::across({{ .cols }}, center))
}

#' @description
#' `class_at()` summarizes the class of the selected columns.
#' @rdname dataframe
#' @export
class_at = function(.data, .cols) {
  x = dplyr::summarize(.data, dplyr::across({{ .cols }}, class))
  unlist(x, use.names = TRUE)
}

#' @description
#' `filter_duplicated()` extracts duplicated rows including first occurrences.
#' @param ...  tidy-select passed to [dplyr::select()]
#' @rdname dataframe
#' @export
filter_duplicated = function(.data, ...) {
  x = if (missing(...)) {
    .data
  } else {
    dplyr::select(.data, ...)
  }
  .data[duplicated(x) | duplicated(x, fromLast = TRUE), ]
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
