#' Package utilities
#'
#' @details
#' `clean_install` is a shortcut of `devtools::clean_dll` and
#' `devtools::install`.
#' @inheritParams devtools::install
#' @rdname package
#' @export
clean_install = function(pkg = ".", upgrade = FALSE, ...) {
  devtools::clean_dll(pkg)
  devtools::install(pkg, upgrade = upgrade, ...)
}
