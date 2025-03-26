#' Handle columns with data.frame and matrix
#'
#' `dedfcol()` and `dedfcol_all()` dissolve data.frame columns in a tibble.
#' They are functionally-limited and faster variants of [tidyr::unpack()].
#' @param .data A data.frame.
#' @param at target column.
#' @param names_sep A separator for the new column names.
#' @rdname column
#' @export
dedfcol = function(.data, at, names_sep = NULL) {
  name = rlang::as_name(rlang::enquo(at))
  subdf = .data[[name]]
  if (!is.null(names_sep)) {
    names(subdf) = paste0(name, names_sep, names(subdf))
  }
  at = match(name, names(.data))
  append_df(.data[-at], subdf, at - 1L)
}

#' @rdname column
#' @export
dedfcol_all = function(.data, names_sep = NULL) {
  idx = vapply(.data, is.data.frame, FALSE, USE.NAMES = FALSE)
  for (at in names(.data)[idx]) {
    .data = dedfcol(.data, !!at, names_sep = names_sep)
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
  cn = names(subdf) %||% seq_along(subdf)
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
