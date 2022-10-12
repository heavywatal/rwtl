#' Utilities for lists
#'
#' `setdefault()` and `update_()` return lists modified in similar ways to
#' the equivalent methods of Python dict.
#' @param object,other A list.
#' @param ... Name-value pairs or tidyselect expressions.
#' @rdname list
#' @export
setdefault = function(object, other = NULL, ...) {
  other = c(other, list(...))
  other = other[!(names(other) %in% names(object))]
  c(object, other)
}

#' @rdname list
#' @export
update_ = function(object, other = NULL, ...) {
  other = c(other, list(...))
  for (key in names(other)) {
    object[[key]] = other[[key]]
  }
  object
}

#' @description `select_list()` is an alternative to [dplyr::select()] for lists.
#' @rdname list
#' @export
select_list = function(object, ...) {
  loc = tidyselect::eval_select(rlang::expr(c(...)), object)
  object[loc]
}
