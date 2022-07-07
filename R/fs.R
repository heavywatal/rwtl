#' Functions related to file systems
#'
#' @details
#' `is_outdated()` tests existence, size, and modification time of files.
#' Note that it returns `TRUE` if a file is empty.
#' @param path files to test
#' @param prerequisites comparables
#' @rdname fs
#' @export
is_outdated = function(path, prerequisites = fs::path()) {
  cond_mtime = if (length(prerequisites) > 0L) {
    mtime(path) < max(mtime(prerequisites))
  } else {
    FALSE
  }
  !fs::file_exists(path) | fs::is_file_empty(path) | cond_mtime
}

mtime = function(path) {
  fs::file_info(path)[["modification_time"]]
}
