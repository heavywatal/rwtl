#' Utilities for data.frame
#'
#' `move_left` moves specified columns to the left.
#' @param .data tibble
#' @param ... colnames or expressions
#' @rdname dataframe
#' @export
move_left = function(.data, ...) {
  dplyr::select(.data, ..., dplyr::everything())
}

#' @description
#' `mutate_left` adds new columns to the left.
#' @rdname dataframe
#' @export
mutate_left = function(.data, ...) {
  dplyr::mutate(.data, ...) %>%
    move_left(names(rlang::quos(...)))
}

#' @description
#' `mutate_jitter` adds random variations to given columns.
#' @param amount magnitude of random variations to add
#' @rdname dataframe
#' @export
mutate_jitter = function(.data, ..., amount) {
  #TODO receive values from ... instead of amount
  dplyr::mutate_at(.data, dplyr::vars(...), function(x) {
    x + stats::runif(length(x), -amount, amount)
  })
}

#' @rdname dataframe
#' @export
center_range = function(.data, ...) {
  dplyr::mutate_at(.data, dplyr::vars(...), function(x) {
    x - mean(range(x))
  })
}

#' @rdname dataframe
#' @export
class_at = function(.data, ...) {
  .data = dplyr::select(.data, ...)
  dplyr::summarize_all(.data, class) %>%
    purrr::flatten_chr() %>%
    stats::setNames(names(.data))
}

#' Shortcut for tidyr::crossing() with repeats.
#' @inheritParams base::rep.int
#' @return tbl
#' @rdname itertools
#' @export
crossing_rep = function(x, times=1L) {
  rep.int(list(x), times) %>%
    stats::setNames(paste0("v", seq_along(.))) %>%
    {
      purrr::invoke(tidyr::crossing, .)
    }
}

#' Get rle (run-length encoding) as tibble.
#' @param x atomic vector
#' @param .name column name in output
#' @return tbl with start and end index columns
#' @rdname rle_df
#' @export
rle_df = function(x, .name="value") {
  x = rle(x)
  tibble::tibble(
    !!.name := x$values,
    start = 0L,
    end = cumsum(x$lengths)
  ) %>%
    dplyr::mutate(start = dplyr::lag(.data$end, 1L, 0L) + 1L)
}
