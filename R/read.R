#' Scan a simple CSV and make a matrix
#' @param filename a string
#' @param what type of data
#' @param sep a string
#' @param skip an integer
#' @param header a logical
#' @return a matrix
#' @rdname read
#' @export
scan_matrix = function(filename, what=double(0), sep="", skip=0L, header=FALSE) {
    p = pipe(paste("wc -l <", file), open="r")
    nlines = scan(p, n=1, quiet=TRUE)
    close(p)
    repeat {
        if (skip>=nlines) {return (NULL)}
        label = scan(file, what=character(), sep=sep, skip=skip, nlines=1, quiet=TRUE, comment.char="#")
        if (length(label)>0) {break} else {skip = skip+1}
    }
    mtrx = matrix(scan(file, what=what, sep=sep, skip=skip+ifelse(header,1,0), quiet=TRUE, comment.char="#"), ncol=length(label), byrow=T)
    if (header) {colnames(mtrx) = label}
    mtrx
}

#' Read an INI-like config file of boost::program_options
#' @return a data.frame
#' @rdname read
#' @export
read_boost_ini = function(filename) {
    readr::read_delim(filename, '=', col_names=c('key', 'val'), comment='#') %>>%
    dplyr::summarise_each(dplyr::funs(paste0(., collapse='\t'))) %>>%
    {paste(.$key, .$val, sep='\n')} %>>%
    readr::read_tsv()
}

#' read paste board into data.frame
#' @param ... passed to read.table()
#' @return a data.frame
#' @rdname read
#' @export
read_cb = function(...) {
    utils::read.table(pipe("pbpaste"), ...) %>>% tibble::as_tibble()
}

#' get commandline arguments
#' @return a list
#' @rdname read
#' @export
command_args = function() {
    .argv = commandArgs(trailingOnly=FALSE)
    l = list()
    l$file = sub('^--file=', '', grep('^--file=', .argv, value=TRUE))
    l$srcdir = dirname(normalizePath(l$file))
    l$args = grep('^[^-]', .argv[-1], value=TRUE)
    return(l)
}
