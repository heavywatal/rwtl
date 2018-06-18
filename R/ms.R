#' Functions to run ms and parse ms-like output
#'
#' @description
#' `ms` executes ms as an external program
#' @param nsam number of samples
#' @param nreps number of repeats
#' @param theta population mutation rate
#' @return `ms` returns a string vector
#' @rdname ms
#' @export
ms = function(nsam=4L, nreps=2L, theta=5.0) {
  args = paste(nsam, nreps, "-t", theta)
  system2("ms", args, stdout = TRUE, stderr = FALSE)
}

#' `parse_ms` transforms ms-like output into a list of matrices
#' @param msout string vector of ms-like output
#' @return `parse_ms` returns a list of integer matrices
#' @rdname ms
#' @export
parse_ms = function(msout) {
  split_ms(msout) %>% purrr::map(as_int_matrix)
}

#' `split_ms` splits ms-like output by replications
#' @return `split_ms` returns a list of string vector
#' @rdname ms
#' @export
split_ms = function(msout) {
  sep_pos = which(msout == "//")
  nreps = length(sep_pos)
  nsam = length(msout) - utils::tail(sep_pos, 1L) - 2L
  utils::tail(msout, -2L) %>%
    split(rep(seq_len(nreps), each = nsam + 4L)) %>%
    purrr::map(utils::tail, -4L)
}

#' `as_int_matrix` converts a string vector into an integer matrix
#' @param samples string vector
#' @return `as_int_matrix` returns an integer matrix
#' @rdname ms
#' @export
as_int_matrix = function(samples) {
  stringr::str_split(samples, "") %>%
    purrr::map(as.integer) %>%
    unlist(recursive = FALSE, use.names = FALSE) %>%
    matrix(ncol = length(samples), byrow = FALSE)
}
