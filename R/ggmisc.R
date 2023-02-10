#' Misc functions with ggplot2
#'
#' `insert_layer()` insert layers to an arbitrary position
#' @param p gg object
#' @param ... layers to add
#' @param after position; 0L is back
#' @examples
#' (ggplot2::ggplot(ggplot2::mpg) +
#'   ggplot2::aes(drv, hwy) +
#'   ggplot2::geom_boxplot(fill = "gold") +
#'   ggplot2::geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 1, stackratio = 0.5) +
#'   ggplot2::stat_summary(fun.data = mean_sd, color = "red", size = 2, linewidth = 2) +
#'   ggplot2::facet_wrap(ggplot2::vars(drv), scale = "free_x", label = label_none)) |>
#'   insert_layer(ggplot2::geom_violin(fill = "darkorange"), after = 1L)
#' @rdname ggmisc
#' @export
insert_layer = function(p, ..., after = 0L) {
  if (after < 0L) {
    after = length(p$layers) + after
  }
  p$layers = append(p$layers, ..., after = after)
  p
}

#' @description
#' `mean_sd()` calculates mean and sd for [ggplot2::stat_summary()].
#' @inheritParams ggplot2::mean_se
#' @rdname ggmisc
#' @export
mean_sd = function(x, mult = 1) {
  x = stats::na.omit(x)
  div = mult * stats::sd(x)
  y = mean(x)
  data.frame(y, ymin = y - div, ymax = y + div)
}

#' @description
#' `label_none()` removes `strip.text` while keeping `strip.background`.
#' @inheritParams ggplot2::label_value
#' @rdname ggmisc
#' @export
label_none = function(labels, ...) {
  lapply(labels, function(x) character(length(x)))
}
