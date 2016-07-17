#' Pythonic method to manupilate strings
#' @param string a character vector
#' @param pattern a separator string as ICU regex
#' @param n the number of splits to do
#' @return a list of character vectors
#' @rdname string
#' @export
rsplit = function(string, pattern, n=42L) {
    pattern = sprintf('%s(?!(?:.*%s){%d})', pattern, pattern, n)
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
