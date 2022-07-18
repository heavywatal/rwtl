#' Utilities for data.frame
#'
#' @description
#' `mutate_left()` adds new columns to the left.
#' @param .data a data.frame.
#' @rdname dataframe
#' @export
mutate_left = function(.data, ...) {
  dplyr::mutate(.data, ...) |>
    dplyr::relocate(names(rlang::quos(...)))
}

#' @description
#' `dedfcol()` and `dedfcol_all()` dissolve data.frame columns in a tibble.
#' @param at target column.
#' @rdname dataframe
#' @export
dedfcol = function(.data, at) {
  name = rlang::as_name(rlang::enquo(at))
  subdf = .data[[name]]
  names(subdf) = paste0(name, "$", names(subdf))
  at = match(name, names(.data))
  append_df(.data[-at], subdf, at - 1L)
}

#' @rdname dataframe
#' @export
dedfcol_all = function(.data) {
  idx = vapply(.data, is.data.frame, FALSE, USE.NAMES = FALSE)
  for (at in names(.data)[idx]) {
    .data = dedfcol(.data, !!at)
  }
  .data
}

#' @description
#' `demtrxcol()` and `demtrxcol_all()` dissolve matrix columns in a tibble.
#' @rdname dataframe
#' @export
demtrxcol = function(.data, at) {
  name = rlang::as_name(rlang::enquo(at))
  mtrx = .data[[name]]
  subdf = split(mtrx, col(mtrx, as.factor = TRUE))
  cn = names(subdf)
  if (is.null(cn)) cn = seq_along(subdf)
  cn = paste0("[,", cn, "]")
  names(subdf) = c(paste0(name, cn[[1]]), cn[[-1]])
  at = match(name, names(.data))
  append_df(.data[-at], subdf, at - 1L)
}

#' @rdname dataframe
#' @export
demtrxcol_all = function(.data) {
  idx = vapply(.data, is.matrix, FALSE, USE.NAMES = FALSE)
  for (at in names(.data)[idx]) {
    .data = demtrxcol(.data, !!at)
  }
  .data
}

append_df = function(x, values, after = length(x)) {
  structure(
    append(x, values, after),
    class = class(x),
    row.names = seq_len(nrow(x))
  )
}

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
  purrr::invoke(tidyr::crossing, x)
}
