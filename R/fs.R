#' Functions related to file systems
#'
#' @description
#' `is_outdated()` tests existence and modification time of files.
#' @param path files to test
#' @param prerequisites comparables
#' @rdname fs
#' @export
is_outdated = function(path, prerequisites = character(0L)) {
  pre_time = if (length(prerequisites) == 0L) {
    -Inf
  } else {
    file.mtime(prerequisites)
  }
  if (length(path) == 1L) {
    pre_time = max(pre_time)
  }
  stopifnot(length(pre_time) %in% c(1L, length(path)))
  !file.exists(path) | (file.mtime(path) < pre_time)
}

#' @description
#' `file_copy_try()` is a modified version of [fs::file_copy()] to copy only new files.
#' @param new_path path to the new file or target directory.
#' @param overwrite If this is FALSE and the file is outdated, a message will be shown.
#' @rdname fs
#' @export
file_copy_try = function(path, new_path, overwrite = FALSE) {
  new_path = path_destination(path, new_path)
  outdated = is_outdated(new_path, path)
  if (isTRUE(overwrite)) {
    idx = outdated
  } else {
    idx = is_outdated(new_path)
    left_outdated = outdated & !idx
    if (any(left_outdated)) {
      message("outdated: ", toString(new_path[left_outdated]))
    }
  }
  fs::file_copy(path[idx], new_path[idx], overwrite = overwrite)
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
