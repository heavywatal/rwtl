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
