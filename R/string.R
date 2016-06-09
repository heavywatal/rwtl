#' Pythonic method to manupilate strings
#' @param string a character vector
#' @param pattern a separator string as ICU regex
#' @param n the number of splits to do
#' @return a list of character vectors
#' @rdname string
#' @export
rsplit = function(string, pattern, n=42) {
    pattern = sprintf('%s(?!(?:.*%s){%d})', pattern, pattern, n)
    stringr::str_split(string, pattern)
}
