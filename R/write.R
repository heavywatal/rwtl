#' Save R object with its name
#' @param obj R object
#' @param dir a string
#' @rdname write
#' @export
save_as = function(obj, dir = ".") {
  obj_name = deparse(substitute(obj))
  filepath = file.path(dir, paste0(obj_name, ".rds"))
  saveRDS(obj, file = filepath)
  cat("wrote: ", filepath, "\n")
}
