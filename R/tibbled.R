#' Tibbled version of base R functions

#' @param x atomic vector
#' @param name column name in output
#' @seealso [rle()]
#' @rdname tibbled
#' @export
rle_df = function(x, name = "value") {
  x = rle(x)
  tibble::tibble(
    !!name := x[["values"]],
    start = 0L,
    end = cumsum(x[["lengths"]])
  ) |>
    dplyr::mutate(start = dplyr::lag(.data[["end"]], 1L, 0L) + 1L)
}

#' @seealso [table()], [dplyr::count()]
#' @rdname tibbled
#' @export
table_df = function(x, name = "n") {
  qx = rlang::enquo(x)
  tab = table(x)
  tibble::lst(!!qx := as.vector(names(tab), mode = mode(x)), !!name := as.integer(tab)) |>
    tibble::new_tibble(nrow = length(tab))
}
