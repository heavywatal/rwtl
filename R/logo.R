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
  lengths = (1 + 2 * expand) * (devsize / min(devsize))
  margin = (lengths - 1) / 2
  xlim = c(0, lengths[1]) - margin[1]
  ylim = c(0, lengths[2]) - margin[2]
  scale = min(devsize) / min(lengths)
  d_swoosh = data.frame(
    x = c(0.01, 0.07, 0.99),
    y = c(0.36, 0.20, 0.72)
  )
  d_path = data.frame(
    x = c(0.35, 0.11, 0.55, 0.41, 0.75, 0.71, 0.95),
    y = c(0.95, 0.05, 0.73, 0.05, 0.51, 0.05, 0.29)
  )
  d_dot = data.frame(x = 0.77, y = 0.77)
  ggplot2::ggplot() +
    ggplot2::aes(.data[["x"]], .data[["y"]]) +
    ggplot2::geom_polygon(data = d_swoosh, fill = swoosh) +
    ggplot2::geom_path(data = d_path, linewidth = 2 * scale, linejoin = "bevel", color = path) +
    ggplot2::geom_point(data = d_dot, size = 5 * scale, shape = 16, stroke = FALSE, color = dot) +
    ggplot2::coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
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
  basename = fs::path_file(filename)
  if (stringr::str_detect(basename, "\\bwhite\\b")) {
    bg = "#ffffff"
  }
  if (stringr::str_detect(basename, "\\bcircle\\b")) {
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
