#' Shortcut to use knitr and rmarkdown from commandline
#'
#' @param input File path.
#' @param output File path.
#' @rdname exec
#' @export
exec_knit = function(input, output = NA) {
  if (!is.na(output)) {
    output = normalizePath(output)
    knitr::opts_knit$set(base.dir = dirname(output))
  }
  setwd(dirname(input))
  knitr::knit(basename(input), output)
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
