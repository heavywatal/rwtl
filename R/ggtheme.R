#' Shortcut for ggplot theme
#'
#' @description
#' `theme_wtl()` is a slightly modified variant of [ggplot2::theme_bw()].
#' - Increase `base_size` default to 12.
#' - Increase the small text size from `rel(0.8)` to `rel(1.0/1.2)`.
#' - Increase the color contrast of `axis.text`.
#' - Decrease the color contrast of `axis.ticks`.
#' - Remove `panel.grid.minor`.
#' - Remove `legend.key` background.
#' - Set default color palettes to `viridis` and `Okabe-Ito`.
#' @inheritParams ggplot2::theme_bw
#' @examples
#' p = ggplot2::ggplot(ggplot2::mpg) +
#'   ggplot2::aes(displ, hwy, color = displ, fill = class) +
#'   ggplot2::geom_point(shape = 21, size = 3, stroke = 2)
#'
#' p + ggplot2::theme_bw()
#' p + theme_wtl()
#' p + theme_wtl() + erase(axis.ticks) + axis_line()
#' @rdname ggtheme
#' @export
theme_wtl = function(base_size = 12, base_family = "", header_family = NULL,
                     base_line_size = base_size / 22, base_rect_size = base_size / 22,
                     ink = "black", paper = "white", accent = "#3366FF") {
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family,
    header_family = header_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size,
    ink = ink,
    paper = paper,
    accent = accent
  ) +
    theme_sub_wtl(ink = ink, paper = paper)
}

theme_sub_wtl = function(
    ink = "black", paper = "white",
    rel = 1.0 / 1.2,
    continuous = NULL, discrete = NULL) {
  theme_sub_misc(ink = ink, paper = paper) +
    theme_sub_text_small(rel = rel) +
    theme_sub_palette(continuous = continuous, discrete = discrete)
}

theme_sub_misc = function(ink = "black", paper = "white") {
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(color = scales::col_mix(ink, paper, 0.25)),
    axis.ticks = ggplot2::element_line(color = scales::col_mix(ink, paper, 0.5)),
    legend.key = ggplot2::element_rect(fill = NA, color = NA)
  )
}

theme_sub_text_small = function(rel = 1.0 / 1.2) {
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = ggplot2::rel(rel)),
    legend.text = ggplot2::element_text(size = ggplot2::rel(rel)),
    strip.text = ggplot2::element_text(size = ggplot2::rel(rel)),
    plot.caption = ggplot2::element_text(size = ggplot2::rel(rel))
  )
}

theme_sub_palette = function(continuous = NULL, discrete = NULL) {
  if (is.null(continuous)) continuous = "viridis"
  if (is.null(discrete)) {
    discrete = grDevices::palette.colors(palette = "Okabe-Ito")[-1]
  }
  ggplot2::theme(
    palette.colour.continuous = continuous,
    palette.fill.continuous = continuous,
    palette.colour.discrete = discrete,
    palette.fill.discrete = discrete
  )
}

#' @description
#' `erase()` is a shortcut for `theme(... = element_blank())`.
#' @param ... The elements to get blank.
#' @param .names A string vector of element names.
#' @rdname ggtheme
#' @export
erase = function(..., .names = NULL) {
  quosures = rlang::quos(...)
  .names = c(vapply(quosures, rlang::quo_name, ""), .names)
  elements = rlang::rep_along(.names, list(ggplot2::element_blank()))
  rlang::exec(ggplot2::theme, !!!rlang::set_names(elements, .names))
}

#' @description
#' `axis_line()` sets L-shaped axes.
#' @param linewidth,linetype Passed to [ggplot2::element_line()].
#' @inheritParams ggplot2::element_line
#' @rdname ggtheme
#' @export
axis_line = function(colour = NULL, linewidth = NULL, linetype = NULL, lineend = NULL,
                     color = NULL, arrow = NULL, arrow.fill = NULL, inherit.blank = FALSE) {
  elem = ggplot2::element_line(
    colour = colour, linewidth = linewidth, linetype = linetype, lineend = lineend,
    color = color, arrow = arrow, arrow.fill = arrow.fill, inherit.blank = inherit.blank
  )
  ggplot2::theme(panel.border = ggplot2::element_blank(), axis.line = elem)
}
