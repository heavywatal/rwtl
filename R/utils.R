#' Simple wrapper of Rprof() and summaryRprof()
#' @param expr R expression
#' @param interval numeric
#' @param memory logical
#' @return summaryRprof()
#' @rdname utils
#' @export
easierprof = function(expr, interval=0.02, memory=FALSE) {
  .tmpfile = tempfile()
  utils::Rprof(.tmpfile, interval = interval, memory.profiling = memory)
  eval(substitute(expr))
  utils::Rprof(NULL)
  utils::summaryRprof(.tmpfile, memory = ifelse(memory, "both", "none"))
}

#' Set width and height according to the current environment
#' @param max.print maximum number of rows to print
#' @rdname utils
#' @export
adjust_print_options = function(max.print = 30L) {
  # COLUMNS and LINES are unreadable during startup
  stty_size = system("stty size", intern = TRUE)
  stopifnot(length(stty_size) > 0L)
  stty_size = strsplit(stty_size, " ")[[1L]]
  tibble_height = min(as.integer(stty_size[1L]) - 6L, max.print)
  options(
    width = as.integer(stty_size[2L]),
    datatable.print.nrows = tibble_height,
    datatable.print.topn = tibble_height %/% 2L,
    tibble.print_max = tibble_height,
    tibble.print_min = tibble_height
  )
  if (interactive()) {
    message("width: ", getOption("width"))
    message("datatable.print.nrows: ", getOption("datatable.print.nrows"))
    message("datatable.print.topn: ", getOption("datatable.print.topn"))
    message("tibble.print_max: ", getOption("tibble.print_max"))
    message("tibble.print_min: ", getOption("tibble.print_min"))
  }
}

#' Shortcut of page(x, method='print')
#' @inheritParams utils::page
#' @param width integer
#' @rdname utils
#' @export
less = function(x, method=c("print", "dput"),
                max.print=getOption("max.print"),
                width=getOption("width"), ...) {
  opts = options(
    max.print = max.print,
    tibble.print_max = max.print,
    tibble.print_min = max.print,
    width = width
  )
  on.exit(options(opts))
  utils::page(x, match.arg(method), ...)
}

#' Print big tibble as it is
#' @rdname utils
#' @export
max_print = function(x, max.print=getOption("max.print"), ...) {
  opts = options(
    max.print = max.print,
    tibble.print_max = max.print,
    tibble.print_min = max.print
  )
  on.exit(options(opts))
  print(x, ...)
}

#' Workaround to achieve devtools::document(..., export_all=FALSE)
#' @param pkg repository name
#' @param dir parent directory of the repository
#' @rdname utils
#' @export
refresh = function(pkg="rwtl", dir="~/git") {
  pkg = file.path(dir, pkg)
  devtools::document(pkg)
  # load_all(pkg) is called in document()
  suppressMessages(devtools::load_all(pkg, export_all = FALSE, quiet = TRUE))
}

#' @rdname utils
#' @export
install_tibble134 = function() {
  devtools::install_version("styler", "1.0.0")
  devtools::install_version("tibble", "1.3.4")
}
