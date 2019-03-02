#' Package utilities
#'
#' @details
#' `clean_install` is a shortcut of `devtools::document` and
#' `devtools::install` without dependency upgrade.
#' @inheritParams devtools::install
#' @rdname package
#' @export
clean_install = function(pkg = ".", upgrade = FALSE, ...) {
  pkgbuild::compile_dll(pkg)
  devtools::document(pkg)
  devtools::install(pkg, upgrade = upgrade, ...)
}

#' @details
#' `chk` and `tst` are thin wrappers of `devtools::check` and `testthat::test_package`.
#' @param install If FALSE, add `--no-install` to `args`
#' @param vignettes If FALSE, add `--ignore-vignettes` to `args`
#' @rdname package
#' @export
chk = function(pkg = ".", install = FALSE, vignettes = FALSE, args = c("--timings"), ...) {
  if (!install) {
    args = c(args, "--no-install")
    if (has_tests()) on.exit(tst(pkg))
  }
  if (!vignettes) {
    args = c(args, "--ignore-vignettes")
  }
  devtools::check(pkg, vignettes = vignettes, args = args, ...)
}

#' @rdname package
#' @export
tst = function(pkg = ".", ...) {
  package = devtools::as.package(pkg)$package
  testthat::test_package(package, ...)
}

has_tests = function(pkg = ".") {
  system.file("tests", package = devtools::as.package(pkg)$package) != ""
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
  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    "binary"
  } else {
    getOption("pkgType")
  }
}
