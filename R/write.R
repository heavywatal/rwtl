#' Save R object with its name
#' @param obj R object
#' @param dir a string
#' @rdname write
#' @export
save_as = function(obj, dir='.') {
    obj_name = as.character(substitute(obj))
    filepath = file.path(dir, paste0(obj_name, '.rda'))
    save(list=obj_name, file=filepath)
    cat('wrote: ', filepath, '\n')
}

#' Write gzipped file with write.table() and gzfile()
#' @inheritParams readr::write_delim
#' @inheritParams utils::write.table
#' @param ... passed to gzfile()
#' @rdname write
#' @export
write_gz_table = function(x, path, delim='\t', na='NA', append=FALSE, col_names=!append, ...) {
    utils::write.table(x, gzfile(path, ...),
        append=append, quote=FALSE, sep=delim, na=na,
        row.names=FALSE, col.names=col_names)
}
