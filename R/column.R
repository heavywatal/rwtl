#' Handle columns with data.frame and matrix
#'
#' `dedfcol()` and `dedfcol_all()` dissolve data.frame columns in a tibble.
#' @param .data A data.frame.
#' @param at target column.
#' @rdname column
#' @export
dedfcol = function(.data, at) {
  name = rlang::as_name(rlang::enquo(at))
  subdf = .data[[name]]
  names(subdf) = paste0(name, "$", names(subdf))
  at = match(name, names(.data))
  append_df(.data[-at], subdf, at - 1L)
}

#' @rdname column
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
#' @rdname column
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

#' @rdname column
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
