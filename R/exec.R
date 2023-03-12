#' Shortcut to use knitr and rmarkdown from commandline
#'
#' @param input File path.
#' @param output File path.
#' @rdname exec
#' @export
exec_knit = function(input, output = NA) {
  knitr::opts_chunk$set(cache.path = ".cache/")
  if (is.na(output)) {
    withr::with_dir(
      dirname(input),
      knitr::knit(basename(input))
    )
  } else {
    input = normalizePath(input)
    withr::with_dir(
      dirname(output),
      knitr::knit(input, basename(output))
    )
  }
}

#' @param output_dir Directory path.
#' @rdname exec
#' @export
exec_render = function(input, output_dir = NA) {
  if (is.na(output_dir)) {
    output_dir = getwd()
  }
  rmarkdown::render(input, output_dir = output_dir)
}

#' @rdname exec
#' @export
DiagrammeR_dot2svg = function(input, output_dir = NA) {
  stopifnot(file.exists(input))
  if (is.na(output_dir)) {
    output_dir = getwd()
  }
  widget = DiagrammeR::grViz(input)
  svg = DiagrammeRsvg::export_svg(widget)
  pattern = stringr::regex(".+(?=<svg)", dotall = TRUE)
  svg = stringr::str_replace(svg, pattern, "")
  outfile = stringr::str_replace(input, "\\.dot$|\\.gr$", ".svg")
  cat(svg, file = fs::path(output_dir, outfile))
}
