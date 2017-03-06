#' Plot pch variations
#' @rdname graphics
#' @export
pch_plot = function() {
    col="red"
    bg="orange"
    graphics::plot(0, 0, xlim=c(0, 6), ylim=c(0, 4),
        type="n", axes=FALSE, xlab="", ylab="")
    a = rep(0:5, rep(5, 6))
    b = c(4:0, 4:0, 4:0, 4:0, 4:0, 4:0)
    graphics::points(a + 0.3, b, pch=0:29, cex=3, col=col, bg=bg)
    graphics::text(a, b, 0:29)
    graphics::mtext(paste0("col='", col, "', bg='", bg, "'"))
}

#' Translate coloname to hex
#' @param color a character vector
#' @return a character vector
#' @rdname graphics
#' @export
col2hex = function(color=grDevices::colors()) {
    grDevices::rgb(t(grDevices::col2rgb(color)), max=255)
}

#' Simple alternative to hist() with ggplot2
#' @param x vector
#' @param ... passed to geom_*()
#' @return gg
#' @rdname graphics
#' @export
gghist = function(x, ...) {
    if (is.double(x)) {
        geom = ggplot2::geom_histogram
    } else {
        geom = ggplot2::geom_bar
    }
    ggplot2::ggplot(tibble::tibble(x=x), ggplot2::aes(x)) + geom(...)
}

#' Save PDF with quartz
#' @inheritParams ggplot2::ggsave
#' @param height,units see ggplot2::ggsave
#' @rdname graphics
#' @export
ggsave_quartz = function(
        filename, plot=ggplot2::last_plot(), device=grDevices::quartz,
        path=NULL, scale=1, width=7, height=7,
        units=c('in', 'cm', 'mm'), dpi=300, limitsize=TRUE) {
    ext = tools::file_ext(filename)
    stopifnot(ext %in% c('png', 'pdf'))
    ggplot2::ggsave(filename, plot, device, path,
         scale, width, height, units, dpi, limitsize, type=ext)
}

#########1#########2#########3#########4#########5#########6#########7#########

#' Axis breaks: scale_*_log10(breaks=breaks_log10)
#' @param limits numeric
#' @rdname axis-label
#' @export
breaks_log10 = function(limits) {
    loglims = log10(limits)
    10 ^ seq(floor(min(loglims)), ceiling(max(loglims)))
}

#' Axis label formatter: scale_*_log10(labels=labels_log10)
#' @param breaks numeric
#' @rdname axis-label
#' @export
labels_log10 = function(breaks) {
    parse(text=paste0('10^', log10(breaks)))
}
