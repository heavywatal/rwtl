#' Pipe text to system commands
#'
#' @examples
#' iris |> less()
#' iris |> egrep(vir)
#' iris |> egrep("vir")
#' letters |> pipeshell(cat)
#'
#' iris %P% wc
#' iris %P% "wc -l"
#' iris %P% `grep seto | less`
#'
#' vir = "seto"
#' iris %G% vir
#' iris %G% !!vir
#' iris %G% `-v seto | less`
#' @inheritParams max_print
#' @description
#' `pipeshell()` and `%P%` send `x` to `command` via shell.
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
`%P%` = function(x, command) {
  pipeshell(x, !!rlang::enquo(command))
}

#' @description
#' `less()` print `x` in `getOption('pager')`. The operation is in-memory and
#' efficient than `page(x, method='print')` that involves a temporary file.
#' @rdname pipe
#' @export
less = function(x, ...) {
  pipeshell(x, command = !!getOption("pager"), ...)
}

#' @description
#' `egrep()` and `%G%` are shorthand for `x %P% "egrep {expr}"`.
#' @param expr arguments to `egrep`; string or expression.
#' @rdname pipe
#' @export
egrep = function(x, expr, ...) {
  expr = rlang::as_name(rlang::enquo(expr))
  command = paste("egrep", expr)
  pipeshell(x, command = !!command, ...)
}

#' @rdname pipe
#' @export
`%G%` = function(x, expr) {
  egrep(x, !!rlang::enquo(expr))
}

sinkpipe = function(expr, command) {
  command = rlang::as_name(rlang::enquo(command))
  withr::with_connection(
    list(file = pipe(command, open = "w")),
    redirect(expr, file)
  )
}

#' @description
#' `redirect()` evaluates `expr` while `sink(file)` is in operation.
#' @inheritParams base::sink
#' @rdname pipe
#' @export
redirect = function(expr, file = "/dev/null") {
  withr::with_output_sink(
    file,
    invisible(eval(expr))
  )
}
