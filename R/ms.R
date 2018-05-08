#' Execute ms as an external program
#' @param nsam number of samples
#' @param nreps number of repeats
#' @param theta population mutation rate
#' @return string vector
#' @rdname ms
#' @export
ms = function(nsam=4L, nreps=2L, theta=5.0) {
  args = paste(nsam, nreps, "-t", theta)
  system2("ms", args, stdout = TRUE, stderr = FALSE)
}

#' Read ms output and return a list of '01' string vector
#' @param lines output of system('ms')
#' @return list of string vector
#' @rdname ms
#' @export
parse_ms = function(lines) {
  sep_pos = which(lines == "//")
  nreps = length(sep_pos)
  nsam = length(lines) - utils::tail(sep_pos, 1L) - 2L
  utils::tail(lines, -2L) %>%
    split(rep(seq_len(nreps), each = nsam + 4L)) %>%
    purrr::map(utils::tail, -4L)
}
