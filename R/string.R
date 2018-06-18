#' Split a single string into a vector
#' @inheritParams stringr::str_split
#' @return character vectors
#' @rdname string
#' @export
split_chr = function(string, pattern="\\s+", n=Inf) {
  stringr::str_split(string, pattern, n) %>% purrr::flatten_chr()
}

#' Pythonic method to manipulate strings
#' @return a list of character vectors
#' @rdname string
#' @export
rsplit = function(string, pattern, n=42L) {
  pattern = sprintf("%s(?!(?:.*%s){%d})", pattern, pattern, n)
  stringr::str_split(string, pattern)
}

#' Pythonic alias of charToRaw()
#' @param char character
#' @rdname string
#' @export
ord = function(char) {
  strtoi(charToRaw(char), 16L)
}

#' Pythonic alias of intToUtf8()
#' @param i integer
#' @rdname string
#' @export
chr = function(i) {
  intToUtf8(i)
}

#' Convert expr into character vector
#' @param ... expressions
#' @rdname string
#' @export
as_chr = function(...) {
  lazyeval::lazy_dots(...) %>%
    purrr::map_chr(~ deparse(.x$expr)) %>%
    stats::setNames(NULL)
}
