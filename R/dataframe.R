#' Move specified columns to left
#' @param .data tibble
#' @param ... colnames or expressions
#' @return tbl
#' @rdname dataframe
#' @export
move_left = function(.data, ...) {
    dplyr::select(.data, ..., dplyr::everything())
}

#' @rdname dataframe
#' @export
mutate_left = function(.data, ...) {
    dplyr::mutate(.data, ...) %>%
    move_left(names(rlang::quos(...)))
}

#' @rdname dataframe
#' @export
class_at = function(.data, ...) {
    .data = dplyr::select(.data, ...)
    dplyr::summarize_all(.data, class) %>%
    purrr::flatten_chr() %>%
    stats::setNames(names(.data))
}

#########1#########2#########3#########4#########5#########6#########7#########

#' Shortcut for tidyr::crossing() with repeats
#' @inheritParams base::rep.int
#' @return tbl
#' @rdname itertools
#' @export
crossing_rep = function(x, times=1L) {
    rep.int(list(x), times) %>%
    stats::setNames(paste0('v', seq_along(.))) %>%
    tidyr::crossing_()
}
