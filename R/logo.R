#' Draw and save the logo
#'
#' See `tools/logo.R` for usage.
#' @param devsize A numeric vector. `c(width, height)`
#' @param expand A numeric.
#' @param swoosh,dot,path Color
#' @rdname logo
#' @export
plot_logo = function(devsize = grDevices::dev.size(), expand = 0,
                     swoosh = "#e08010", dot = "#a4321a", path = "#222222") {
  if (length(devsize) == 1L) {
    devsize = rep(devsize, 2L)
  }
  stopifnot(length(expand) == 1L)
  .lengths = (1 + 2 * expand) * (devsize / min(devsize))
  margin = (.lengths - 1) / 2
  xlim = 108 * (c(0, .lengths[1]) - margin[1])
  ylim = 108 * (c(0, .lengths[2]) - margin[2])
  .scale = min(devsize) / min(.lengths)
  d_swoosh = data.frame(
    x = c(5, 11, 103),
    y = c(68, 84, 32)
  )
  d_path = data.frame(
    x = c(39, 15, 59, 45, 79, 75, 99),
    y = c(9, 99, 31, 99, 53, 99, 75)
  )
  d_dot = data.frame(x = 81, y = 27)
  ggplot2::ggplot() +
    ggplot2::aes(.data[["x"]], .data[["y"]]) +
    ggplot2::geom_polygon(data = d_swoosh, fill = swoosh, linetype = 0, linewidth = 0) +
    ggplot2::geom_path(data = d_path, linewidth = 2 * .scale, linejoin = "bevel", color = path) +
    ggplot2::geom_point(data = d_dot, size = 4.65 * .scale, shape = 16, stroke = FALSE, color = dot) +
    ggplot2::coord_fixed(xlim = xlim, ylim = rev(ylim), expand = FALSE) +
    ggplot2::theme_void()
}

#' @param width,height Size in inches.
#' @param midground Layers to insert behind the logo.
#' @inheritParams ggplot2::ggsave
#' @rdname logo
#' @export
save_logo = function(filename, height = 4, dpi = 300, ..., expand = 0,
                     bg = NULL, midground = NULL, width = height,
                     swoosh = "#e08010", dot = "#a4321a", path = "#222222") {
  .basename = fs::path_file(filename)
  if (stringr::str_detect(.basename, "\\bwhite\\b")) {
    bg = "#ffffff"
  }
  if (stringr::str_detect(.basename, "\\bcircle\\b")) {
    expand = max(expand, 0.125)
  }
  p = plot_logo(c(width, height), expand, swoosh = swoosh, dot = dot, path = path)
  if (!is.null(midground)) {
    p = insert_layer(p, midground)
  }
  ggplot2::ggsave(filename, p, width = width, height = height, dpi = dpi, bg = bg, ...)
}

#' @rdname logo
#' @export
save_sticker = function(filename, height = 4, dpi = 300, ...) {
  width = height * sqrt(3) / 2
  hex = annotate_regpolygon(6L, 0.77,
    x = 0.5, y = 0.5, linewidth = height,
    fill = "#ffffff", color = "#888888"
  )
  save_logo(filename, width = width, height = height, expand = 0.191, dpi = dpi, midground = hex, ...)
}
