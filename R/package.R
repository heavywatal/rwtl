#' Package utilities
#'
#' @details
#' `document_install()` is a quick shortcut of [devtools::install()]
#' following `devtools::document()`.
#' @inheritParams devtools::install
#' @rdname package
#' @export
document_install = function(pkg = ".", build = FALSE, upgrade = FALSE, ...) {
  pkgbuild::compile_dll(pkg)
  devtools::document(pkg)
  devtools::install(pkg, build = build, upgrade = upgrade, ...)
}

#' @details
#' `chk()` and `tst()` are thin wrappers of [devtools::check()] and [testthat::test_local()].
#' @param install If FALSE, add `--no-install` to `args`
#' @param vignettes If FALSE, add `--ignore-vignettes` to `args`
#' @rdname package
#' @export
chk = function(pkg = ".", install = FALSE, vignettes = FALSE, args = "--timings", ...) {
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
  withr::with_envvar(
    c(LANG = "en_US.UTF-8"),
    testthat::test_local(pkg, ...)
  )
}

has_tests = function(pkg = ".") {
  pkg = tryCatch(devtools::as.package(pkg), error = function(e) NULL)
  !is.null(pkg) && (system.file("tests", package = pkg$package) != "")
}

#' @details
#' `bioc_install()` and `bioc_valid()` are thin wrappers of BiocManager functions.
#' @inherit BiocManager::install params return title
#' @rdname bioc
#' @export
bioc_install = function(..., update = FALSE, ask = interactive(), type = binary_if_macos()) {
  withr::with_options(
    pkgType = type,
    BiocManager::install(..., update = update, ask = ask)
  )
}

#' @inheritParams BiocManager::valid
#' @rdname bioc
#' @export
bioc_valid = function(..., type = binary_if_macos()) {
  v = BiocManager::valid(..., type = type)
  if (isTRUE(v)) {
    return(v)
  }
  message("too_new:")
  print(v$too_new)
  message("out_of_date:")
  v$out_of_date[, c("LibPath", "Installed", "ReposVer", "Repository")]
}

binary_if_macos = function() {
  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    "binary"
  } else {
    getOption("pkgType")
  }
}
