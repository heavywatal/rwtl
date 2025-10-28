#' Functions to read files
#'
#' @description
#' `read_boost_ini()` reads an INI-like config file of boost::program_options.
#' @source <https://www.boost.org/doc/libs/latest/doc/html/program_options.html>
#' @param file filename or text
#' @returns A tibble or character vector.
#' @rdname read
#' @export
read_boost_ini = function(file) {
  tsv = readr::read_delim(file, "=", col_names = c("key", "val"), comment = "#", trim_ws = TRUE) |>
    dplyr::summarize(dplyr::across(tidyselect::everything(), \(x) paste(x, collapse = "\t"))) |>
    paste(collapse = "\n")
  readr::read_tsv(I(tsv))
}

#' @description
#' `read_fai()` reads a FASTA/FASTQ index file.
#' @source <https://www.htslib.org/doc/faidx.html>
#' @rdname read
#' @export
read_fai = function(file) {
  cn = c("name", "length", "offset", "linebases", "linewidth", "qualoffset")
  readr::read_tsv(file, col_names = cn)
}

#' @description
#' `read_pb()` reads paste board to a character vector.
#' @rdname read
#' @export
read_pb = function() {
  readr::read_lines(I(pipe("pbpaste")))
}
