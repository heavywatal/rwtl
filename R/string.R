#' String utility
#'
#' `split_chr` splits a string and return a flattened vector.
#' @inheritParams stringr::str_split
#' @rdname string
#' @export
split_chr = function(string, pattern = "\\s+", n = Inf) {
  stringr::str_split(string, pattern, n) %>% purrr::flatten_chr()
}

#' @details
#' `rsplit` splits a string from right.
#' @rdname string
#' @export
rsplit = function(string, pattern, n = 42L) {
  pattern = sprintf("%s(?!(?:.*%s){%d})", pattern, pattern, n)
  stringr::str_split(string, pattern)
}

#' @details
#' `ord` is a pythonic alias of `charToRaw()`.
#' @param char character
#' @rdname string
#' @export
ord = function(char) {
  strtoi(charToRaw(char), 16L)
}

#' @details
#' `chr` is a pythonic alias of `intToUtf8()`.
#' @param i integer
#' @rdname string
#' @export
chr = function(i) {
  intToUtf8(i)
}

#' @details
#' `as_chr` converts expr into character vector.
#' @param ... expressions
#' @rdname string
#' @export
as_chr = function(...) {
  lazyeval::lazy_dots(...) %>%
    purrr::map_chr(~ deparse(.x$expr)) %>%
    stats::setNames(NULL)
}
