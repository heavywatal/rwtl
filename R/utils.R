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

#' Set width and height according to the current environment
#' @param max.print maximum number of rows to print
#' @rdname utils
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

#' Print big tibble as it is
#' @inheritParams base::print
#' @rdname utils
#' @export
max_print = function(x, max.print=getOption("max.print"), width=Inf, ...) {
  opts = print_options(width, max.print)
  on.exit(options(opts))
  print.tbl_df = data.table:::print.data.table
  print.tbl = data.table:::print.data.table
  print(x, ...)
}

#' Shortcut of page(x, method='print')
#' @param width integer
#' @rdname utils
#' @export
less = function(x, max.print=getOption("max.print"), width=Inf, ...) {
  file = tempfile("Rpage.")
  sink(file)
  max_print(x, max.print = max.print, width = width, ...)
  sink()
  file.show(file, delete.file = TRUE)
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
