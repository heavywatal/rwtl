#' String utility
#'
#' @description
#' `str_c_coalesce()` tries [stringr::str_c()] first,
#' and falls back to [dplyr::coalesce()].
#' @inheritParams stringr::str_c
#' @rdname string
#' @export
str_c_coalesce = function(..., sep = "", collapse = NULL) {
  dplyr::coalesce(
    stringr::str_c(..., sep = sep, collapse = collapse),
    dplyr::coalesce(...)
  )
}

#' @description
#' `rsplit()` splits a string from right.
#' @inheritParams stringr::str_split
#' @rdname string
#' @export
rsplit = function(string, pattern = "\\s+", n = 42L) {
  pattern = sprintf("%s(?!(?:.*%s){%d})", pattern, pattern, n)
  stringr::str_split(string, pattern)
}

#' @description
#' `as_chr()` converts expr into character vector.
#' @param ... expressions
#' @rdname string
#' @export
as_chr = function(...) {
  rlang::quos(...) |>
    purrr::map_chr(rlang::as_name) |>
    unname()
}

#' @description
#' `as_code()` converts a vector into R code that generates it.
#' @param x vector
#' @rdname string
#' @export
as_code = function(x) {
  if (is.character(x)) {
    x = paste0('"', x, '"')
  }
  x = toString(x)
  paste0("c(", x, ")")
}

#' Conversion between character and integer code point
#'
#' @description
#' `ord()` is a pythonic alias of `charToRaw()`.
#' @param char character
#' @rdname chr
#' @export
ord = function(char) {
  strtoi(charToRaw(char), 16L)
}

#' @description
#' `chr()` is a pythonic alias of `intToUtf8()`.
#' @param i integer
#' @rdname chr
#' @export
chr = function(i) {
  intToUtf8(i)
}
