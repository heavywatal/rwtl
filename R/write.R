#' Save R object with its name
#' @param x R object
#' @param dir a string
#' @rdname write
#' @export
save_as = function(x, dir='.') {
    obj_name = as.character(substitute(x))
    filepath = file.path(dir, paste0(obj_name, '.rda'))
    save(list=obj_name, file=filepath)
    cat('wrote: ', filepath, '\n')
}
