#' get commandline arguments
#' @return a list
#' @rdname command
#' @export
command_args = function() {
    .argv = commandArgs(trailingOnly=FALSE)
    l = list()
    l$file = sub('^--file=', '', grep('^--file=', .argv, value=TRUE))
    l$srcdir = dirname(normalizePath(l$file))
    l$args = grep('^[^-]', .argv[-1], value=TRUE)
    return(l)
}

#' script path in Rscript
#' @return a character; NULL in interactive mode
#' @rdname command
#' @export
this_file = function() {
    args = commandArgs(trailingOnly=FALSE)
    file_arg = grep("--file=", args, value=TRUE)
    stringr::str_replace(file_arg, '^--file=', '')
}
