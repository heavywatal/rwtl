#' Simple wrapper of Rprof() and summaryRprof()
#' @param expr R expression
#' @param interval numeric
#' @param memory logical
#' @return summaryRprof()
#' @rdname utils
#' @export
easierprof = function(expr, interval=0.02, memory=FALSE) {
     .tmpfile = tempfile()
     utils::Rprof(.tmpfile, interval=interval, memory.profiling=memory)
     eval(substitute(expr))
     utils::Rprof(NULL)
     utils::summaryRprof(.tmpfile, memory= ifelse(memory, 'both', 'none'))
}
