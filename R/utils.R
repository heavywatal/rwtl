#' Utility functions
#'
#' `refresh` is a shortcut for `devtools::document(..., export_all=FALSE)`
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

#' `easierprof` is a simple wrapper of `Rprof()` and `summaryRprof()`
#' @param expr R expression
#' @param interval numeric
#' @param memory logical
#' @rdname utils
#' @export
easierprof = function(expr, interval=0.02, memory=FALSE) {
  .tmpfile = tempfile()
  utils::Rprof(.tmpfile, interval = interval, memory.profiling = memory)
  eval(substitute(expr))
  utils::Rprof(NULL)
  utils::summaryRprof(.tmpfile, memory = ifelse(memory, "both", "none"))
}
