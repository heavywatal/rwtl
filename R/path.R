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
