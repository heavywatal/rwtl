#' A shortcut
#' @param ... character vectors conatining file paths
#' @return a logical vector
#' @rdname path
#' @export
isdir = function(...) file.info(...)$isdir

#' os.path.splitext() in Python
#' @param path a string
#' @return a character vector (root, ext)
#' @rdname path
#' @export
splitext = function(path) {
    unlist(rsplit(path, "\\.", 1))
}

#' script path in Rscript
#' @return a character; NULL in interactive mode
#' @rdname path
#' @export
this_file = function() {
    args = commandArgs(trailingOnly=FALSE)
    file_arg = grep("--file=", args, value=TRUE)
    stringr::str_replace(file_arg, '^--file=', '')
}
