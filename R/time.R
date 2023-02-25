#' Snippet for time in R
#' @param format string
#' @rdname time
#' @export
str_today = function(format = "%Y%m%d") {
  base::format(Sys.time(), format)
}

#' @rdname time
#' @export
str_now = function(format = "%Y%m%d_%H%M%S") {
  base::format(Sys.time(), format)
}
