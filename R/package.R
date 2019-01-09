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
  devtools::document(pkg)
  devtools::install(pkg, upgrade = upgrade, ...)
}

#' @details
#' `bioc_valid` and `bioc_install` are thin wrappers of BiocManager functions.
#' @inheritParams utils::update.packages
#' @rdname package
#' @export
bioc_valid = function(..., type = binary_if_macos()) {
  v = BiocManager::valid(..., type = type)
  message("too_new:")
  print(v$too_new)
  message("out_of_date:")
  v$out_of_date[, c("LibPath", "Installed", "ReposVer", "Repository")]
}

#' @rdname package
#' @export
bioc_install = function(..., ask = interactive(), type = binary_if_macos()) {
  original = options(pkgType = type)
  on.exit(options(original))
  BiocManager::install(..., ask = ask)
}

binary_if_macos = function() {
  if (identical(Sys.info()[["sysname"]], "Darwin")) "binary"
  else getOption("pkgType")
}
