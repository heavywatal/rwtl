#' source('*/biocLite.R') and biocLite()
#' @param mirror URL for BioC_mirror
#' @param ask passed to biocLite()
#' @rdname bioc
#' @export
library_bioc = function(mirror=getOption("BioC_mirror"), ask=FALSE) {
  source(sprintf("%s/biocLite.R", mirror))
  BiocInstaller::biocLite(ask = ask)
}
