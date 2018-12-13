#' Utility functions
#'
#' @details
#' `easierprof` is a simple wrapper of `Rprof()` and `summaryRprof()`.
#' @param expr R expression
#' @param interval numeric
#' @param memory logical
#' @rdname utils
#' @export
easierprof = function(expr, interval = 0.02, memory = FALSE) {
  .tmpfile = tempfile()
  utils::Rprof(.tmpfile, interval = interval, memory.profiling = memory)
  eval(substitute(expr))
  utils::Rprof(NULL)
  utils::summaryRprof(.tmpfile, memory = ifelse(memory, "both", "none"))
}

#' @details
#' `getenv` converts a result of `Sys.getenv()` to data.frame.
#' @param pattern R expression
#' @rdname utils
#' @export
getenv = function(pattern = NULL) {
  env = Sys.getenv()
  class(env) = "character"
  env = tibble::tibble(key = names(env), value = env)
  if (!is.null(pattern)) {
    env = dplyr::filter(env, stringr::str_detect(.data$key, pattern))
  }
  env
}
