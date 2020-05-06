#' Variants of `ggplot2::aes()` to ignore errors in evaluation
#'
#' @param .data data.frame
#' @param ... passed to `ggplot2::aes()`
#' @rdname ggaes
#' @export
#' @examples
#' ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width)) +
#'   ggplot2::geom_point(aes_try(iris, colour = Species, fill = NOEXIST))
aes_try = function(.data, ...) {
  .aes_try_impl(ggplot2::aes, .data, ...)
}

#' @rdname ggaes
#' @export
aes_try_ = function(.data, ...) {
  .aes_try_impl(ggplot2::aes_, .data, ...)
}

#' @rdname ggaes
#' @export
aes_try_string = function(.data, ...) {
  .aes_try_impl(ggplot2::aes_string, .data, ...)
}

.aes_try_impl = function(.function, .data, ...) {
  mapping = .function(...)
  is_ok = vapply(mapping, function(.x) {
    .x = rlang::quo_set_env(.x, emptyenv())
    tryCatch(
      {
        rlang::eval_tidy(.x, data = .data)
        TRUE
      },
      error = function(e) FALSE
    )
  }, logical(1L))
  mapping[is_ok]
}
