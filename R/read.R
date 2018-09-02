#' Functions to read files
#'
#' `scan_matrix` scans a simple CSV and make a matrix.
#' @param filename a string
#' @param what type of data
#' @param sep a string
#' @param skip an integer
#' @param header a logical
#' @rdname read
#' @export
scan_matrix = function(filename, what=double(0), sep="", skip=0L, header=FALSE) {
  p = pipe(paste("wc -l <", file), open = "r")
  nlines = scan(p, n = 1, quiet = TRUE)
  close(p)
  repeat {
    if (skip >= nlines) {
      return(NULL)
    }
    label = scan(file, what = character(), sep = sep, skip = skip, nlines = 1, quiet = TRUE, comment.char = "#")
    if (length(label) > 0) {
      break
    } else {
      skip = skip + 1
    }
  }
  mtrx = matrix(scan(file, what = what, sep = sep, skip = skip + ifelse(header, 1, 0), quiet = TRUE, comment.char = "#"), ncol = length(label), byrow = T)
  if (header) {
    colnames(mtrx) = label
  }
  mtrx
}

#' @description
#' `read_boost_ini` reads an INI-like config file of boost::program_options.
#' @param file filename or text
#' @rdname read
#' @export
read_boost_ini = function(file) {
  readr::read_delim(file, "=", col_names = c("key", "val"), comment = "#", trim_ws = TRUE) %>%
    dplyr::summarise_all(function(x) paste0(x, collapse = "\t")) %>%
    paste0(collapse="\n") %>%
    readr::read_tsv()
}

#' @description
#' `read_cb` reads paste board into data.frame.
#' @param ... passed to `read.table()`
#' @rdname read
#' @export
read_cb = function(...) {
  utils::read.table(pipe("pbpaste"), ...) %>% tibble::as_tibble()
}
