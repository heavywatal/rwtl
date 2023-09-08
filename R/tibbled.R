#' Tibbled version of base R functions
#'
#' `rle_df()` returns a tibbled rle.
#' @param x,... Atomic vector
#' @seealso [rle()]
#' @rdname tibbled
#' @export
rle_df = function(x) {
  tibble::new_tibble(rle(x))
}

#' @description `rle_ranges()` returns an IRanges-like tibble.
#' @rdname tibbled
#' @export
rle_ranges = function(...) {
  qx = rlang::enquos(..., .named = TRUE)
  name = names(qx)[[1L]]
  x = rle(c(..., use.names = FALSE))
  end = cumsum(x[["lengths"]])
  start = c(0L, end[-length(end)]) + 1L
  tibble::new_tibble(tibble::lst(start, end, {{ name }} := x[["values"]]))
}

#' @description `table_df()` returns a tibbled table.
#' @param name The name of the new column in the output.
#' @seealso [table()], [dplyr::count()]
#' @rdname tibbled
#' @export
table_df = function(..., name = "n") {
  qx = rlang::enquos(..., .named = TRUE)
  x = rlang::eval_tidy(qx[[1L]])
  tab = table(x)
  lhs = names(qx)[[1L]]
  rhs = as.vector(names(tab), mode = typeof(x))
  tibble::new_tibble(tibble::lst(!!lhs := rhs, !!name := as.integer(tab)))
}

#' @description `tabulate0_df()` is a tabulated variant of `table_df()`.
#' @seealso [tabulate()]
#' @rdname tibbled
#' @export
tabulate0_df = function(..., name = "n") {
  qx = rlang::enquos(..., .named = TRUE)
  x = rlang::eval_tidy(qx[[1L]])
  tab = tabulate0(x)
  lhs = names(qx)[[1L]]
  rhs = seq.int(0L, max(x, na.rm = TRUE))
  tibble::new_tibble(tibble::lst(!!lhs := rhs, !!name := as.integer(tab)))
}

#' @description
#' `tabulate0()` is a variant of [tabulate()] to count integers including zero.
#' @inheritParams base::tabulate
#' @rdname tibbled
#' @export
tabulate0 = function(bin, nbins = max(1L, bin, na.rm = TRUE)) {
  tabulate(bin + 1L, nbins = nbins + 1L)
}
