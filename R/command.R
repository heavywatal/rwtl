#' Parse command line arguments
#'
#' @description
#' `command_args` is an extention of `commandArgs()`.
#' @rdname command
#' @export
command_args = function() {
  .argv = commandArgs(trailingOnly = FALSE)
  l = list()
  l$file = sub("^--file=", "", grep("^--file=", .argv, value = TRUE))
  l$srcdir = dirname(normalizePath(l$file))
  l$args = grep("^[^-]", .argv[-1], value = TRUE)
  return(l)
}

#' @description
#' `this_file` extracts script path from `commandArgs()`.
#' @rdname command
#' @export
this_file = function() {
  args = commandArgs(trailingOnly = FALSE)
  file_arg = grep("--file=", args, value = TRUE)
  stringr::str_replace(file_arg, "^--file=", "")
}
