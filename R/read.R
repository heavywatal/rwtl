#' Functions to read files
#'
#' @description
#' `read_boost_ini()` reads an INI-like config file of boost::program_options.
#' @param file filename or text
#' @rdname read
#' @export
read_boost_ini = function(file) {
  tsv = readr::read_delim(file, "=", col_names = c("key", "val"), comment = "#", trim_ws = TRUE) |>
    dplyr::summarize(dplyr::across(tidyselect::everything(), \(x) paste0(x, collapse = "\t"))) |>
    paste0(collapse = "\n")
  readr::read_tsv(I(tsv))
}

#' @description
#' `read_pb()` reads paste board into data.frame.
#' @rdname read
#' @export
read_pb = function() {
  readr::read_file(pipe("pbpaste"))
}
