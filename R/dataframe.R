#' Move specified columns to left
#' @param .data tbl
#' @param ... colnames or expressions
#' @param .dots vector of strings or expressions
#' @rdname dataframe
#' @export
move_left_ = function(.data, ..., .dots) {
    dplyr::select_(.data, ..., ~dplyr::one_of(.dots), ~dplyr::everything())
}

#' @rdname dataframe
#' @export
move_left = function(.data, ...) {
    .dots = purrr::map_chr(lazyeval::lazy_dots(...), ~deparse(.x$expr))
    move_left_(.data, .dots=.dots)
}

#' @rdname dataframe
#' @export
mutate_left_ = function(.data, .dots) {
    dplyr::mutate_(.data, .dots=.dots) %>>%
    move_left_(.dots=names(.dots))
}

#' @rdname dataframe
#' @export
mutate_left = function(.data, ...) {
    mutate_left_(.data, lazyeval::lazy_dots(...))
}
