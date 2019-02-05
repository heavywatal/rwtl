#' Utilities for printing and piping
#'
#' @examples
#' \dontrun{
#' max_print(mpg)
#'
#' mpg %>% less()
#' mpg %>% grepp(manual)
#' mpg %>% grepp("manual")
#' mpg %>% pipeshell(wc)
#'
#' iris %|% wc
#' iris %|% "wc -l"
#' iris %|% `grep seto | less`
#'
#' vir = "seto"
#' iris %G% vir
#' iris %G% !!vir
#' iris %G% `-v seto | less`
#' }
#' @param x an object to print
#' @param max.print maximum number of rows to print
#' @param width maximum number of columns to print
#' @param ... further arguments passed to `print`
#' @details
#' `max_print` prints as many elements in a big tibble as possible.
#' @rdname print
#' @export
max_print = function(x, max.print = getOption("max.print"), width = Inf, ...) {
  opts = print_options(height = max.print, width = width, max.print = max.print)
  on.exit(options(opts))
  print(x, ...)
}

#' @details
#' `pipeshell` and `%|%` send `x` to `command` via shell.
#' @param command to which the text is sent; string or expression.
#' @rdname print
#' @export
pipeshell = function(x, command, ...) UseMethod("pipeshell")

#' @export
pipeshell.default = function(x, command, ...) {
  command = rlang::enquo(command)
  sinkpipe(print(x, ...), !!command)
}

#' @export
pipeshell.data.frame = function(x, command, ..., max.print = getOption("max.print"), width = Inf) {
  command = rlang::enquo(command)
  sinkpipe(max_print(x, max.print = max.print, width = width, ...), !!command)
}

#' @rdname print
#' @export
`%|%` = function(x, command) {
  pipeshell(x, !!rlang::enquo(command))
}

#' @details
#' `less` print `x` in `getOption('pager')`. The operation is in-memory and
#' efficient than `page(x, method='print')` that involves a temporary file.
#' @rdname print
#' @export
less = function(x, ...) {
  pipeshell(x, command = !!getOption('pager'), ...)
}

#' @details
#' `grepp` and `%G%` are shorthand for `x %|% "grep -P {expr}"`.
#' @param expr arguments to `grep -P`; string or expression.
#' @rdname print
#' @export
grepp = function(x, expr, ...) {
  expr = rlang::as_name(rlang::enquo(expr))
  command = paste("grep -P", expr)
  pipeshell(x, command = !!command, ...)
}

#' @rdname print
#' @export
`%G%` = function(x, expr) {
  grepp(x, !!rlang::enquo(expr))
}

sinkpipe = function(expr, command) {
  command = rlang::as_name(rlang::enquo(command))
  file = pipe(command, open = "w")
  on.exit(close(file))
  redirect(expr, file)
}

#' @details
#' `redirect` evaluates `expr` while `sink(file)` is in operation.
#' @inheritParams base::sink
#' @rdname print
#' @export
redirect = function(expr, file = "/dev/null") {
  on.exit(sink(NULL))
  sink(file)
  invisible(eval(expr))
}

#' @details
#' `adjust_print_options` sets width and height according to the current environment.
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

print_options = function(height, width, ...) {
  options(
    datatable.print.nrows = height,
    datatable.print.topn = height %/% 2L,
    tibble.print_max = height,
    tibble.print_min = height,
    tibble.width = width,
    width = min(width, 10000L),
    ...
  )
}
