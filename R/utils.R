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

#' Set options(width) according to the current environment
#' @param width integer
#' @rdname utils
#' @export
adjust_width = function(width=Sys.getenv("COLUMNS")) {
    if (width == '') {
        if (Sys.getenv("RSTUDIO") == "1") {return()}
        stty = system("stty -a", intern=TRUE, ignore.stderr=TRUE)[1]
        if (is.na(stty)) {return()}
        colmuns = grep("columns", unlist(strsplit(stty, ";")), value=TRUE)
        width = grep("\\d+", unlist(strsplit(colmuns, " ")), value=TRUE)
    }
    options(width=width)
    if (interactive()) {message('width: ', getOption('width'))}
}
