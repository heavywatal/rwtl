#' Parse command line arguments
#'
#' @details
#' `command_args` is an extention of `commandArgs()`.
#' @rdname command
#' @export
command_args = function() {
  argv = commandArgs(trailingOnly = FALSE)
  x = list()
  x$file = sub("^--file=", "", grep("^--file=", argv, value = TRUE))
  x$srcdir = dirname(normalizePath(x$file))
  x$args = grep("^[^-]", argv[-1], value = TRUE)
  x
}
