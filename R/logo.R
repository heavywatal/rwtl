#' Draw and save the logo
#'
#' See `tools/logo.R` for usage.
#' @param devsize A numeric vector. `c(width, height)`
#' @param expand A numeric.
#' @param swoosh,dot,path Color
#' @rdname logo
#' @export
logo_plot = function(
  devsize = grDevices::dev.size(),
  expand = 0,
  swoosh = "#E08010",
  dot = "#A4321A",
  path = "#202020"
) {
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
    ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed(xlim = xlim, ylim = rev(ylim), expand = FALSE) +
    ggplot2::theme_void()
}

#' @param width,height Size in inches.
#' @param midground Layers to insert behind the logo.
#' @inheritParams ggplot2::ggsave
#' @rdname logo
#' @export
logo_save = function(
  filename,
  height = 1.5,
  dpi = 300,
  ...,
  expand = 0,
  bg = NULL,
  midground = NULL,
  width = height,
  swoosh = "#E08010",
  dot = "#A4321A",
  path = "#202020"
) {
  .basename = fs::path_file(filename)
  if (stringr::str_detect(.basename, "\\bwhite\\b")) {
    bg = "#FFFFFF"
  }
  if (stringr::str_detect(.basename, "\\bcircle\\b")) {
    expand = max(expand, 0.125)
  }
  p = logo_plot(c(width, height), expand, swoosh = swoosh, dot = dot, path = path)
  if (!is.null(midground)) {
    p = insert_layer(p, midground)
  }
  ggplot2::ggsave(filename, p, width = width, height = height, dpi = dpi, bg = bg, ...)
}

#' @rdname logo
#' @export
logo_save_sticker = function(filename, height = 4, dpi = 300, ...) {
  z = 54
  r = 80
  delta_r = 6
  height = height * r / z
  width = height * sqrt(3) / 2
  xp = (r / z * sqrt(3) / 2 - 1) / 2
  hex_outer = annotate_regpolygon(6L, r, x = z, y = z, linewidth = 0, fill = "#808080", color = NA)
  hex_inner = annotate_regpolygon(6L, r - delta_r, x = z, y = z, linewidth = 0, fill = "#FFFFFF", color = NA)
  hex = list(hex_outer, hex_inner)
  logo_save(filename, width = width, height = height, expand = xp, dpi = dpi, midground = hex, ...)
}

#' @rdname logo
#' @export
logo_svg_optimize = function(filename, path = "#F0F0F0") {
  lns = readr::read_lines(filename) |>
    stringr::str_subset("^<rect ", negate = TRUE) |>
    stringr::str_flatten(collapse = "\n") |>
    stringr::str_remove_all("\\.00\\b") |>
    stringr::str_remove_all("\\.0000\\d+") |>
    stringr::str_remove_all("pt\\b") |>
    .svg_remove_clip_path() |>
    .svg_replace_style(stroke_dark = path)
  readr::write_lines(lns, filename)
  invisible(filename)
}

.svg_remove_clip_path = function(x) {
  patt = stringr::regex("<defs>\\s*<clipPath.+?/clipPath>\\s*</defs>\n*", dotall = TRUE)
  x = stringr::str_remove(x, patt)
  x = stringr::str_remove(x, " clip-path='[^']+'")
  x
}

.svg_replace_style = function(x, stroke_dark) {
  patt = "(<polyline.+?) *stroke: *([^;]+);"
  mat = stringr::str_match(x, patt)
  stroke = mat[1, 3]
  x = stringr::str_replace(x, patt, "\\1")
  # fmt: skip
  repl = glue("
  .svglite polyline {{
    fill: none;
    stroke: {stroke};
  }}
  @media (prefers-color-scheme: dark) {{
    .svglite polyline {{
      stroke: {stroke_dark};
    }}
  }}
  ")
  patt = stringr::regex("(?<=\\[CDATA\\[).+?(?=\\]\\])", dotall = TRUE)
  x = stringr::str_replace(x, patt, repl)
  x
}
