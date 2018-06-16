#' Configure print options.
#'
#' @param x an object to print
#' @param width maximum number of columns to print
#' @param max.print maximum number of rows to print
#' @param ... further arguments passed to `print`

#' @description
#' `adjust_print_options` sets width and height according to the current environment
#' @rdname print
#' @export
adjust_print_options = function(max.print = 30L) {
  # COLUMNS and LINES are unreadable during startup
  stty_size = system("stty size", intern = TRUE)
  message("stty size: ", stty_size)
  stty_size = as.integer(strsplit(stty_size, " ")[[1L]])
  stopifnot(length(stty_size) > 1L)
  stopifnot(stty_size > 10L)
  print_options(
    height = min(stty_size[1L] - 6L, max.print),
    width = stty_size[2L]
  )
  if (interactive()) {
    message("datatable.print.nrows: ", getOption("datatable.print.nrows"))
    message("datatable.print.topn: ", getOption("datatable.print.topn"))
    message("tibble.print_max: ", getOption("tibble.print_max"))
    message("tibble.print_min: ", getOption("tibble.print_min"))
    message("width: ", getOption("width"))
  }
}

#' `max_print` prints a big tibble as it is
#' @rdname print
#' @export
max_print = function(x, max.print=getOption("max.print"), width=Inf, ...) {
  opts = print_options(height = max.print, width = width)
  on.exit(options(opts))
  print(x, ...)
}

#' `less` is a shortcut of page(x, method='print')
#' @rdname print
#' @export
less = function(x, max.print=getOption("max.print"), width=Inf, ...) {
  file = tempfile("Rpage.")
  sink(file)
  max_print(x, max.print = max.print, width = width, ...)
  sink()
  file.show(file, delete.file = TRUE)
}

print_options = function(height, width) {
  options(
    datatable.print.nrows = height,
    datatable.print.topn = height %/% 2L,
    tibble.print_max = height,
    tibble.print_min = height,
    tibble.width = width,
    width = min(width, 10000L)
  )
}
