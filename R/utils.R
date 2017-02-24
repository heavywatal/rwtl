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

#' Shortcut of page(x, method='print')
#' @inheritParams utils::page
#' @param max.print maximum number of rows to print
#' @rdname utils
#' @export
less = function(x, method=c('print', 'dput'), max.print=getOption('max.print'), width=getOption('width'), ...) {
    opts = options(max.print=max.print,
            tibble.print_max=max.print,
            tibble.print_min=max.print,
            width=width)
    on.exit(options(opts))
    utils::page(x, match.arg(method), ...)
}

#' Print big tibble as it is
#' @rdname utils
#' @export
max_print = function(x, max.print=getOption('max.print'), ...) {
    opts = options(max.print=max.print,
            tibble.print_max=max.print,
            tibble.print_min=max.print)
    on.exit(options(opts))
    print(x, ...)
}

#' Workaround to achieve devtools::document(..., export_all=FALSE)
#' @param pkg repository name
#' @param dir parent directory of the repository
#' @rdname utils
#' @export
refresh = function(pkg='rwtl', dir='~/git') {
    pkg = file.path(dir, pkg)
    devtools::document(pkg)
    # load_all(pkg) is called in document()
    suppressMessages(devtools::load_all(pkg, export_all=FALSE, quiet=TRUE))
}
