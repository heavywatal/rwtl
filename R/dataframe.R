#' Utilities for data.frame
#'
#' @details
#' `move_left()` moves specified columns to the left.
#' @param .data tibble
#' @param ... colnames or expressions
#' @rdname dataframe
#' @export
move_left = function(.data, ...) {
  dplyr::select(.data, ..., dplyr::everything())
}

#' @details
#' `mutate_left()` adds new columns to the left.
#' @rdname dataframe
#' @export
mutate_left = function(.data, ...) {
  dplyr::mutate(.data, ...) |>
    move_left(names(rlang::quos(...)))
}

#' @details
#' `dedfcol()` and `dedfcol_all()` dissolve data.frame columns in a tibble.
#' @param at target column
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

#' @details
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

#' Get rle (run-length encoding) as tibble.
#' @param x atomic vector
#' @param .name column name in output
#' @return tbl with start and end index columns
#' @seealso [rle()]
#' @rdname rle_df
#' @export
rle_df = function(x, .name = "value") {
  x = rle(x)
  tibble::tibble(
    !!.name := x[["values"]],
    start = 0L,
    end = cumsum(x[["lengths"]])
  ) |>
    dplyr::mutate(start = dplyr::lag(.data[["end"]], 1L, 0L) + 1L)
}

#' `table_df()` counts elements in a vector.
#' @param ... atomic vector
#' @rdname rle_df
#' @export
#' @seealso [table()]
table_df = function(..., .name = "n") {
  ls = list(...)
  df = tibble::new_tibble(ls, nrow = length(ls[[1L]]))
  dplyr::count(df, !!!rlang::syms(names(ls)), name = .name)
}
