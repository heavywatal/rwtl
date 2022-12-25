#' Parallel execution in the purrr::map style
#'
#' @description
#' `mcmap()` is a variant of [parallel::mclapply()] that accepts a formula as `.f`.
#' @inheritParams purrr::map
#' @param .mc.cores integer
#' @rdname parallel
#' @export
mcmap = function(.x, .f, ..., .mc.cores = getOption("mc.cores", 2L)) {
  parallel::mclapply(.x, rlang::as_function(.f), ..., mc.cores = .mc.cores)
}

#' @rdname parallel
#' @export
mcmap_lgl = function(.x, .f, ..., .mc.cores = getOption("mc.cores", 2L)) {
  purrr::list_simplify(mcmap(.x, .f, ..., .mc.cores = .mc.cores), ptype = logical(1L))
}

#' @rdname parallel
#' @export
mcmap_int = function(.x, .f, ..., .mc.cores = getOption("mc.cores", 2L)) {
  purrr::list_simplify(mcmap(.x, .f, ..., .mc.cores = .mc.cores), ptype = integer(1L))
}

#' @rdname parallel
#' @export
mcmap_dbl = function(.x, .f, ..., .mc.cores = getOption("mc.cores", 2L)) {
  purrr::list_simplify(mcmap(.x, .f, ..., .mc.cores = .mc.cores), ptype = double(1L))
}

#' @rdname parallel
#' @export
mcmap_chr = function(.x, .f, ..., .mc.cores = getOption("mc.cores", 2L)) {
  purrr::list_simplify(mcmap(.x, .f, ..., .mc.cores = .mc.cores), ptype = character(1L))
}
