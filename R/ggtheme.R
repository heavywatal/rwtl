#' Shortcut for ggplot theme
#'
#' @description
#' `theme_wtl` is a slightly modified `theme_bw`.
#' @param base_size,base_family see ggplot2::ggtheme
#' @rdname ggtheme
#' @export
theme_wtl = function(base_size = 12, base_family = "") {
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = "grey10"),
      legend.key = ggplot2::element_rect(fill = NA, colour = NA)
    )
}

#' @description
#' `erase` is a shortcut for `theme(... = element_blank())`.
#' @param ... elements to be blank
#' @param .names string vector of element names
#' @rdname ggtheme
#' @export
erase = function(..., .names = NULL) {
  quosures = rlang::quos(...)
  names = c(sapply(quosures, rlang::quo_name), .names)
  elements = rlang::rep_along(names, list(ggplot2::element_blank()))
  rlang::invoke(ggplot2::theme, rlang::set_names(elements, names))
}

#' @description
#' `axis_line` sets L-shaped axes.
#' @inheritParams ggplot2::element_line
#' @rdname ggtheme
#' @export
axis_line = function(colour = NULL, size = NULL, linetype = NULL, lineend = NULL, arrow = NULL, inherit.blank = FALSE) {
  el = ggplot2::element_line(colour = colour, size = size, linetype = linetype, lineend = lineend, arrow = arrow, inherit.blank = inherit.blank)
  ggplot2::theme(panel.border = ggplot2::element_blank(), axis.line = el)
}
