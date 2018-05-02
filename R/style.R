#' Apply tidyverse_style, but use equal sign = for assignment
#' @rdname style
#' @export
eq_assign_style = function(...) {
  x = styler::tidyverse_style(...)
  x$token$force_assignment_op = function(pd) {
    to_replace = (pd$token == "LEFT_ASSIGN" & pd$text == "<-")
    pd$token[to_replace] = "EQ_ASSIGN"
    pd$text[to_replace] = "="
    pd
  }
  x
}

#' @inheritParams styler::style_pkg
#' @inheritParams styler::style_dir
#' @rdname style
#' @export
styler_style = function(path=".", ..., style = eq_assign_style, filetype = "R",
                        recursive = TRUE, exclude_files = NULL) {
  if (fs::is_dir(path)) {
    .pkg_files = c("DESCRIPTION", "NAMESPACE", "R")
    if (all(fs::file_exists(fs::path(path, .pkg_files)))) {
      styler::style_pkg(
        path, ..., style = style, filetype = filetype,
        exclude_files = c(exclude_files, "R/RcppExports.R")
      )
    } else {
      styler::style_dir(
        path, ..., style = style, filetype = filetype, recursive = recursive,
        exclude_files = exclude_files
      )
    }
  } else {
    styler::style_file(path, ..., style = style)
  }
}
