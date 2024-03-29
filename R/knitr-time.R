#' Register knitr hook to time chunks
#'
#' @param enable A logical.
#' @rdname knitr-time
#' @export
knit_hooks_set_timeit = function(enable = TRUE) {
  knitr::knit_hooks$set(timeit = hook_time_chunk)
  knitr::opts_chunk$set(timeit = enable)
}

hook_time_chunk = function(before, options, envir) {
  if (before) {
    envir$.start = proc.time()
  } else {
    dt = round(proc.time() - envir$.start, 2)
    msg = paste0(options$label, ": ", dt[3], " (", toString(dt[-3]), ")")
    cat(msg, file = stderr(), sep = "\n")
  }
}
