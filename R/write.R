#' Save R object with its name
#' @param obj R object
#' @param dir a string
#' @rdname write
#' @export
save_as = function(obj, dir='.') {
    obj_name = deparse(substitute(obj))
    filepath = file.path(dir, paste0(obj_name, '.rds'))
    saveRDS(obj, file=filepath)
    cat('wrote: ', filepath, '\n')
}

#' Write commpressed file according to the filename
#' @inheritParams readr::write_delim
#' @inheritParams utils::write.table
#' @param ... passed to `*zfile()`
#' @rdname write
#' @export
write_df = function(x, path, delim='\t', na='NA', append=FALSE, col_names=!append, ...) {
    file = if (grepl('gz$', path)) {
        gzfile(path, ...)
    } else if (grepl('bz2$', path)) {
        bzfile(path, ...)
    } else if (grepl('xz$', path)) {
        xzfile(path, ...)
    } else {path}
    utils::write.table(x, file,
        append=append, quote=FALSE, sep=delim, na=na,
        row.names=FALSE, col.names=col_names)
}
