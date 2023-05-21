#' Functions related to file systems
#'
#' @description
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

#' @description
#' `file_copy_try()` is a modified version of [fs::file_copy()] not to raise
#' errors when targets exist.
#' @param new_path path to the new file or target directory.
#' @rdname fs
#' @export
file_copy_try = function(path, new_path) {
  new_path = path_destination(path, new_path)
  outdated = is_outdated(new_path)
  fs::file_copy(path[outdated], new_path[outdated])
  invisible(new_path)
}

#' @description
#' `path_destination()` normalizes destination paths.
#' @rdname fs
#' @export
path_destination = function(path, new_path) {
  if (length(new_path) == 1L && fs::is_dir(new_path)) {
    new_path = rep(new_path, length(path))
  }
  is_dir = fs::is_dir(new_path)
  new_path[is_dir] = fs::path(new_path[is_dir], basename(path[is_dir]))
  stopifnot(length(path) == length(new_path))
  fs::path(new_path)
}
