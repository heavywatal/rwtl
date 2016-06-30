#' Wrapper of brewer.pal()
#' @inheritParams RColorBrewer::brewer.pal
#' @examples
#' ggplot2::scale_colour_gradientn(colours=brewer_palette('RdBu'))
#' @rdname palette
#' @export
brewer_palette = function(name, n=0L) {
    if (missing(name)) {
        return(RColorBrewer::brewer.pal.info)
    }
    if (!name %in% rownames(RColorBrewer::brewer.pal.info)) {
        print(RColorBrewer::brewer.pal.info)
        #RColorBrewer::display.brewer.all()
        stop(sprintf('"%s" not in brewer.pal', name))
    }
    info = RColorBrewer::brewer.pal.info[name,]
    if (n < 1) {n = info$maxcolors}
    RColorBrewer::brewer.pal(n, name)
}

#' Shortcut of scale_colour_gradientn(colours=brewer.pal(n, name))
#' @inheritParams RColorBrewer::brewer.pal
#' @inheritParams ggplot2::scale_colour_gradientn
#' @param reverse logical
#' @rdname palette
#' @export
scale_brewer_colour = function(name, n=0L, reverse=FALSE, ...) {
    pal = brewer_palette(name, n)
    if (reverse) {pal = rev(pal)}
    ggplot2::scale_colour_gradientn(colours=pal, ...)
}

#' Shortcut of scale_fill_gradientn(colours=brewer.pal(n, name))
#' @inheritParams RColorBrewer::brewer.pal
#' @inheritParams ggplot2::scale_fill_gradientn
#' @rdname palette
#' @export
scale_brewer_fill = function(name, n=0L, reverse=FALSE, ...) {
    pal = brewer_palette(name, n)
    if (reverse) {pal = rev(pal)}
    ggplot2::scale_fill_gradientn(colours=pal, ...)
}
