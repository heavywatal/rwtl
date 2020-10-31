#' String utility
#'
#' @details
#' `join` concatenats strings in a vector.
#' @inheritParams stringr::str_c
#' @param na.rm logical
#' @rdname string
#' @export
join = function(string, sep = "", na.rm = FALSE) {
  if (isTRUE(na.rm)) string = stats::na.omit(string)
  stringr::str_c(string, collapse = sep)
}

#' @details
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
rsplit = function(string, pattern = "\\s+", n = 42L) {
  pattern = sprintf("%s(?!(?:.*%s){%d})", pattern, pattern, n)
  stringr::str_split(string, pattern)
}

#' Conversion between character and integer code point
#'
#' @details
#' `ord` is a pythonic alias of `charToRaw()`.
#' @param char character
#' @rdname chr
#' @export
ord = function(char) {
  strtoi(charToRaw(char), 16L)
}

#' @details
#' `chr` is a pythonic alias of `intToUtf8()`.
#' @param i integer
#' @rdname chr
#' @export
chr = function(i) {
  intToUtf8(i)
}

#' @details
#' `as_chr` converts expr into character vector.
#' @param ... expressions
#' @rdname chr
#' @export
as_chr = function(...) {
  lazyeval::lazy_dots(...) %>%
    purrr::map_chr(~ deparse(.x$expr)) %>%
    stats::setNames(NULL)
}

#' `as_code` converts a vector into R code that generates it.
#' @param x vector
#' @rdname as_code
#' @export
as_code = function(x) {
  if (is.character(x)) {
    x = paste0('"', x, '"')
  }
  x = paste(x, collapse = ", ")
  paste0("c(", x, ")")
}
