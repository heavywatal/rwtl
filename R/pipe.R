#' Pipe text to system commands
#'
#' @examples
#' \dontrun{
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
#' @inheritParams max_print
#' @details
#' `pipeshell` and `%|%` send `x` to `command` via shell.
#' @param command to which the text is sent; string or expression.
#' @rdname pipe
#' @export
pipeshell = function(x, command, ...) UseMethod("pipeshell")

#' @export
pipeshell.default = function(x, command, ...) {
  command = rlang::enquo(command)
  sinkpipe(print(x, ...), !!command)
}

#' @export
pipeshell.data.frame = function(x, command, ..., n = getOption("max.print"), width = Inf) {
  command = rlang::enquo(command)
  sinkpipe(max_print(x, n = n, width = width, ...), !!command)
}

#' @rdname pipe
#' @export
`%|%` = function(x, command) {
  pipeshell(x, !!rlang::enquo(command))
}

#' @details
#' `less` print `x` in `getOption('pager')`. The operation is in-memory and
#' efficient than `page(x, method='print')` that involves a temporary file.
#' @rdname pipe
#' @export
less = function(x, ...) {
  pipeshell(x, command = !!getOption('pager'), ...)
}

#' @details
#' `grepp` and `%G%` are shorthand for `x %|% "grep -P {expr}"`.
#' @param expr arguments to `grep -P`; string or expression.
#' @rdname pipe
#' @export
grepp = function(x, expr, ...) {
  expr = rlang::as_name(rlang::enquo(expr))
  command = paste("grep -P", expr)
  pipeshell(x, command = !!command, ...)
}

#' @rdname pipe
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
#' @rdname pipe
#' @export
redirect = function(expr, file = "/dev/null") {
  on.exit(sink(NULL))
  sink(file)
  invisible(eval(expr))
}
