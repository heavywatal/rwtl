#' Package development utilities
#'
#' @description
#' `chk()` is a thin wrapper of [devtools::check()].
#' @inheritParams rcmdcheck::rcmdcheck
#' @param install If FALSE, add `--no-install` to `args`
#' @param vignettes If FALSE, add `--ignore-vignettes` to `args`
#' @param ... Passed to [rcmdcheck::rcmdcheck()].
#' @rdname package-dev
#' @export
chk = function(path = ".", install = FALSE, vignettes = FALSE, args = "--timings", ...) {
  if (!install) {
    args = c(args, "--no-install")
    if (has_tests(path)) on.exit(testthat::test_local(path))
  }
  if (!vignettes) {
    args = c(args, "--ignore-vignettes")
  }
  rcmdcheck::rcmdcheck(path, args = args, ...)
}

has_tests = function(path = ".") {
  withr::local_dir(path)
  res = tryCatch(testthat::test_path("_"), error = \(e) NULL)
  !is.null(res)
}

#' Package management utilities
#'
#' @description
#' `lib_upgrade()` is a stopgap until `pak::lib_upgrade()` <https://github.com/r-lib/pak/issues/168>.
#' There is no direct way to enforce binary for now: <https://github.com/r-lib/pak/issues/318>.
#' `pak::pkg_upgrade()` is not available yet: <https://github.com/r-lib/pak/pull/289>.
#' Versioned CRAN packages are not implemented yet <https://github.com/r-lib/pkgdepends/blob/main/R/type-cran.R>.
#' @param ... Passed to [utils::old.packages()] or [pak::pkg_install()].
#' @param binary A logical. Set `FALSE` to accept source packages.
#' @inheritParams pak::pkg_install
#' @rdname package
#' @export
lib_upgrade = function(..., binary = TRUE, ask = interactive()) {
  old_pkgs = old_packages(..., binary = binary)
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
      try(install_packages(pkgs, lib = LibPath, ask = ask, binary = binary))
    })
}

#' @description
#' `old_packages()` is a thin wrapper of [utils::old.packages()].
#' @rdname package
#' @export
old_packages = function(..., binary = TRUE) {
  if (isTRUE(binary)) {
    # RSPM recommends using the default type (not "binary")
    withr::local_options(
      pkgType = .Platform$pkgType,
      install.packages.check.source = "no",
      install.packages.compile.from.source = "no"
    )
  }
  out_of_date = utils::old.packages(...)
  if (is.null(out_of_date)) {
    return(invisible())
  }
  as.data.frame(out_of_date) |>
    tibble::new_tibble() |>
    dplyr::relocate("Installed", "ReposVer", .after = "Package")
}

#' @description
#' `install_packages()` is a thin wrapper of [pak::pkg_install()]
#' to temporarily force binary installation.
#' @rdname package
#' @export
install_packages = function(pkg, lib = .libPaths()[[1L]], ..., binary = TRUE) {
  binary = binary && grepl("binary", .Platform$pkgType, fixed = TRUE)
  platform = if (binary) R.version$platform else NULL
  withr::local_options(pkg.platforms = platform)
  if ("cmdstanr" %in% pkg) {
    pak::repo_add(stan = "https://stan-dev.r-universe.dev")
  }
  pak::pkg_install(pkg, lib = lib, ...)
}
