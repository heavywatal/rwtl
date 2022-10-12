#' Functions for knitr fig.process
#'
#' `oxipng()` losslessly improves compression of PNG files.
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
#' @rdname figprocess
#' @export
cwebp = function(input, options = NULL) {
  defaults = list(
    quiet = TRUE, mt = TRUE, lossless = TRUE, o = fs::path_ext_set(input, "webp")
  )
  opts = figprocess(input, "cwebp", options, defaults, "-")
  fs::file_delete(input)
  invisible(opts$cwebp.o)
}

figprocess = function(input, command, options = NULL, defaults = NULL, prefix = "--") {
  names(defaults) = paste(command, names(defaults), sep = ".")
  pattern = paste0("^", command, "\\.")
  options = setdefault(options, defaults) |>
    select_list(tidyselect::matches(pattern)) |>
    purrr::keep(~ !isFALSE(.x))
  keys = stringr::str_replace(names(options), pattern, prefix)
  values = as.character(options)
  args = matrix(c(keys, values), nrow = 2L, byrow = TRUE) |>
    as.vector() |>
    stringr::str_subset("^TRUE$", negate = TRUE)
  system2(command, c(args, input))
  options
}
