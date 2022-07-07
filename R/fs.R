#' Functions related to file systems
#'
#' @details
#' `is_outdated()` tests existence, size, and modification time of files.
#' Note that it returns `TRUE` if a file is empty.
#' @param path files to test
#' @param prerequisites comparables
#' @rdname fs
#' @export
is_outdated = function(path, prerequisites = character(0L)) {
  cond_mtime = if (length(prerequisites) > 0L) {
    file.mtime(path) < max(file.mtime(prerequisites))
  } else {
    FALSE
  }
  !file.exists(path) | fs::is_file_empty(path) | cond_mtime
}
