#' Parallel execution in the purrr style
#'
#' `mcmap*()` are simple wrappers around [parallel::mclapply()] that allow the
#' usage similar to the [purrr::map()] family.
#' @param .x A vector or list to be mapped over.
#' @param .f A function or formula to be applied to each element of `.x`.
#' @param ... Additional arguments passed to [parallel::mclapply()].
#' @returns A list or vector of the same length as `.x`.
#' [purrr::list_simplify()] is used to convert the result to a vector.
#' `mcwalk()` returns the input `.x` invisibly.
#' @seealso <https://furrr.futureverse.org/>
#' @rdname parallel
#' @export
mcmap = function(.x, .f, ...) {
  parallel::mclapply(.x, purrr::as_mapper(.f), ...)
}

#' @rdname parallel
#' @export
mcmap_lgl = function(.x, .f, ...) {
  mcmap_vec(.x, .f, ..., .ptype = logical(0L))
}

#' @rdname parallel
#' @export
mcmap_int = function(.x, .f, ...) {
  mcmap_vec(.x, .f, ..., .ptype = integer(0L))
}

#' @rdname parallel
#' @export
mcmap_dbl = function(.x, .f, ...) {
  mcmap_vec(.x, .f, ..., .ptype = double(0L))
}

#' @rdname parallel
#' @export
mcmap_chr = function(.x, .f, ...) {
  mcmap_vec(.x, .f, ..., .ptype = character(0L))
}

#' @param .ptype A prototype to specify the output type.
#' @rdname parallel
#' @export
mcmap_vec = function(.x, .f, ..., .ptype = NULL) {
  out = mcmap(.x, .f, ...)
  purrr::list_simplify(out, ptype = .ptype)
}

#' @rdname parallel
#' @export
mcwalk = function(.x, .f, ...) {
  mcmap(.x, .f, ...)
  invisible(.x)
}
