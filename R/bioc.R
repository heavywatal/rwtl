#' source('*/biocLite.R') and biocLite()
#' @param mirror URL for BioC_mirror
#' @param ask passed to biocLite()
#' @rdname bioc
#' @export
library_bioc = function(
    mirror=c('bioc.ism.ac.jp', 'bioconductor.riken.jp', 'bioconductor.org'),
    ask=FALSE) {
    mirror = match.arg(mirror)
    options(BioC_mirror=sprintf('https://%s/', mirror))
    source(sprintf('https://%s/biocLite.R', mirror))
    BiocInstaller::biocLite(ask=ask)
}
