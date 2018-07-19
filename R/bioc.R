#' Shortcut for `source()` and `biocLite()`
#' @param mirror URL for BioC_mirror
#' @param ... passed to `biocLite()`
#' @rdname bioc
#' @export
library_bioc = function(mirror=getOption("BioC_mirror", "https://bioconductor.org"), ...) {
  source(sprintf("%s/biocLite.R", mirror))
  BiocInstaller::biocLite(...)
}
