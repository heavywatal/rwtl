#' Functions to run ms and parse ms-like output
#'
#' @details
#' `ms` executes ms as an external program.
#' @param nsam number of samples
#' @param nreps number of repeats
#' @param theta population mutation rate
#' @return `ms` returns a string vector
#' @rdname ms
#' @export
ms = function(nsam = 4L, nreps = 2L, theta = 5.0) {
  args = paste(nsam, nreps, "-t", theta)
  system2("ms", args, stdout = TRUE, stderr = FALSE)
}

#' @details
#' `parse_ms` transforms ms-like output into a list of matrices.
#' @param msout string vector of ms-like output
#' @param byrow logical value passed to `matrix`
#' @return `parse_ms` returns a list of integer matrices
#' @rdname ms
#' @export
parse_ms = function(msout, byrow = FALSE) {
  split_ms(msout) %>% lapply(as_int_matrix, byrow = byrow)
}

#' @details
#' `split_ms` splits ms-like output by replications.
#' @return `split_ms` returns a list of string vector
#' @rdname ms
#' @export
split_ms = function(msout) {
  sep_pos = which(msout == "//")
  nreps = length(sep_pos)
  nsam = length(msout) - utils::tail(sep_pos, 1L) - 2L
  utils::tail(msout, -2L) %>%
    split(rep(seq_len(nreps), each = nsam + 4L)) %>%
    lapply(utils::tail, -4L)
}

#' @details
#' `as_int_matrix` converts a string vector into an integer matrix.
#' @param samples string vector
#' @return `as_int_matrix` returns an integer matrix
#' @rdname ms
#' @export
as_int_matrix = function(samples, byrow = FALSE) {
  v = stringr::str_split(samples, "") %>%
    lapply(as.integer) %>%
    unlist(recursive = FALSE, use.names = FALSE)
  if (byrow) {
    matrix(v, nrow = length(samples), byrow = TRUE)
  } else {
    matrix(v, ncol = length(samples), byrow = FALSE)
  }
}
