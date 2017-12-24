#' Snippet for time in R
#' @param format string
#' @return string
#' @rdname time
#' @export
today = function(format="%Y%m%d") {
  base::format(Sys.time(), format)
}

#' @return string
#' @rdname time
#' @export
now = function(format="%Y%m%d_%H%M%S") {
  base::format(Sys.time(), format)
}
