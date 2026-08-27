#' Functions for knitr fig.process
#'
#' `oxipng()` losslessly improves compression of PNG files.
#'
#' WebP is the most efficient format for lossless compression of plots and diagrams.
#' AVIF is primarily used for lossy compression of photographic images.
#' @returns Path to the output file.
#' @rdname figprocess
#' @export
oxipng = function(input, options = NULL) {
  defaults = list(quiet = TRUE)
  opts = figprocess(input, "oxipng", options, defaults, "--")
  invisible(opts$oxipng.out %||% fs::path(opts$oxipng.dir %||% ".", input))
}

#' @description
#' `cwebp()` compresses a raster image to WebP format.
#' @param input A path to an image file.
#' @param options A list.
#' @param keep A logical value. Set to `TRUE` to keep the input file.
#' @rdname figprocess
#' @export
cwebp = function(input, options = NULL, keep = FALSE) {
  defaults = list(
    quiet = TRUE,
    mt = TRUE,
    lossless = TRUE,
    o = fs::path_ext_set(input, "webp")
  )
  opts = figprocess(input, "cwebp", options, defaults, "-")
  if (!keep) {
    fs::file_delete(input)
  }
  invisible(opts$cwebp.o)
}

#' @description
#' `avifenc()` compresses a raster image to AVIF format.
#' @rdname figprocess
#' @export
avifenc = function(input, options = NULL, keep = FALSE) {
  defaults = list(
    lossless = TRUE,
    output = fs::path_ext_set(input, "avif")
  )
  opts = figprocess(input, "avifenc", options, defaults, "--")
  if (!keep) {
    fs::file_delete(input)
  }
  invisible(opts$avifenc.output)
}

figprocess = function(input, command, options = NULL, defaults = NULL, prefix = "--") {
  names(defaults) = paste(command, names(defaults), sep = ".")
  pattern = paste0("^", command, "\\.")
  options = setdefault(options, defaults) |>
    select_list(tidyselect::matches(pattern)) |>
    purrr::discard(isFALSE)
  keys = stringr::str_replace(names(options), pattern, prefix)
  values = as.character(options)
  .args = matrix(c(keys, values), nrow = 2L, byrow = TRUE) |>
    as.vector() |>
    stringr::str_subset("^TRUE$", negate = TRUE)
  .stdout = if (command == "avifenc") NULL else ""
  system2(command, c(.args, input), stdout = .stdout)
  options
}
