#' Write R objects to files
#'
#' `save_as()` saves R object with its name.
#' @param obj An R object.
#' @param dir A string.
#' @rdname write
#' @export
save_as = function(obj, dir = ".") {
  obj_name = deparse(substitute(obj))
  filepath = file.path(dir, paste0(obj_name, ".rds"))
  saveRDS(obj, file = filepath)
  cat("wrote: ", filepath, "\n")
}

#' @description
#' `write_table()` is a shortcut of [readr::write_delim()] with decent default options.
#' @inheritParams readr::write_delim
#' @inheritDotParams readr::write_delim
#' @rdname write
#' @export
write_table = function(x, file, delim = "\t", na = "", quote = "needed", ...) {
  readr::write_delim(x, file, delim = delim, na = na, quote = quote, ...)
}
