#' Package utilities
#'
#' @description
#' `document_install()` is a quick shortcut of [devtools::install()]
#' following [devtools::document()].
#' @inheritParams devtools::install
#' @rdname package
#' @export
document_install = function(pkg = ".", build = FALSE, upgrade = FALSE, ...) {
  pkgbuild::compile_dll(pkg)
  devtools::document(pkg)
  devtools::install(pkg, build = build, upgrade = upgrade, ...)
}

#' @description
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

#' @description
#' `update_packages()` is a stopgap until `pak::lib_upgrade()` <https://github.com/r-lib/pak/issues/168>.
#' Set `PKG_PLATFORMS` before loading pak: <https://github.com/r-lib/pak/issues/412>.
#' There is no direct way to enforce binary for now: <https://github.com/r-lib/pak/issues/318>.
#' `pak::pkg_upgrade()` is not available yet: <https://github.com/r-lib/pak/pull/289>.
#' Versioned CRAN packages are not implemented yet <https://github.com/r-lib/pkgdepends/blob/main/R/type-cran.R>.
#' @param bioc A logical.
#' @inheritParams pak::pkg_install
#' @rdname package
#' @export
update_packages = function(..., bioc = FALSE, ask = interactive()) {
  old_pkgs = old_packages(..., bioc = bioc)
  if (is.null(old_pkgs)) {
    return(invisible())
  }
  old_pkgs |>
    dplyr::group_nest(.data$LibPath, .data$Repository) |>
    purrr::pwalk(function(LibPath, Repository, data) {
      pkgs = data[["Package"]]
      cat("LibPath:", LibPath, "\n")
      cat("Repository:", Repository, "\n")
      cat("Package:", pkgs, "\n")
      try(pak::pkg_install(pkgs, lib = LibPath, upgrade = FALSE, ask = ask))
    })
}

#' @description
#' `old_packages()` is a thin wrapper of [utils::old.packages()].
#' @rdname package
#' @export
old_packages = function(..., bioc = FALSE) {
  if (bioc) {
    v = BiocManager::valid(...) # slow
    if (isTRUE(v)) {
      return(invisible())
    }
    message("too_new:")
    print(v$too_new)
    out_of_date = v$out_of_date
  } else {
    out_of_date = utils::old.packages(...)
  }
  if (is.null(out_of_date)) {
    return(invisible())
  }
  as.data.frame(out_of_date) |>
    tibble::new_tibble() |>
    dplyr::relocate(.data$Installed, .data$ReposVer, .after = .data$Package)
}

#' @description
#' `bioc_install()` is a thin wrapper of [BiocManager::install()].
#' @inherit BiocManager::install params return title
#' @rdname bioc
#' @export
bioc_install = function(pkgs, ..., update = FALSE, ask = interactive()) {
  BiocManager::install(pkgs, ..., update = update, ask = ask)
}
