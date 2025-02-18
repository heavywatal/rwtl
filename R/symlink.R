#' Utility functions to handle symlinks
#'
#' @description
#' `symlink()` is a simple wrapper of [fs::link_create()].
#' Existing links are replaced if link paths are different.
#' @param path A character vector.
#' @param target A character vector. If an existing directory is given, links are created there.
#' @param follow A logical vector. `FALSE` means `ln -h` on macOS and `ln -n` on Linux.
#' @rdname symlink
#' @export
symlink = function(path, target, follow = TRUE) {
  if (length(target) == 1L && length(path) > 1L) {
    target = rep(target, length(path))
  }
  new_path = ifelse(fs::dir_exists(target) & follow, fs::path(target, fs::path_file(path)), target)
  on.exit(link_resolve(new_path))
  existing = fs::link_exists(new_path)
  if (any(existing)) {
    existing_target = new_path[existing]
    ln_path = fs::link_path(existing_target)
    msg_lines = paste0(existing_target, "@ -> ", ln_path, collapse = "\n")
    message("Link already exists:\n", msg_lines, "\n")
    is_different = (path[existing] != ln_path)
    if (any(is_different)) {
      message("Replacing: ", toString(existing_target[is_different]))
      fs::link_delete(existing_target[is_different])
    }
  }
  fs::link_create(path, new_path)
}

#' @description
#' `link_resolve()` is a non-stop variant of [fs::path_real()].
#' @rdname symlink
#' @export
link_resolve = function(path) {
  ln_path = fs::link_path(path)
  is_abs = fs::is_absolute_path(ln_path)
  resolved = ifelse(is_abs, ln_path, fs::path(fs::path_dir(path), ln_path))
  for (i in which(!fs::file_exists(resolved))) {
    warning("broken link: ", path[i], "@ -> ", ln_path[i], call. = FALSE)
  }
  fs::path_norm(resolved)
}

symlink_executable = function(src = symlink_latest_library(), dst = "~/local/bin") {
  pkg_path = find.package("wtl", lib.loc = src)
  scripts = fs::dir_ls(fs::path(pkg_path, "exec"))
  symlink(scripts, dst)
}

symlink_latest_library = function(src = Sys.getenv("R_LIBS_USER")) {
  target = fs::path(fs::path_dir(src), "latest")
  symlink(fs::path_file(src), target, follow = FALSE)
}
