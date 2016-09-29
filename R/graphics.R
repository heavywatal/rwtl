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

#' Incomplete theme that does not overwrite other elements
#' @inheritParams ggplot2::ggtheme
#' @rdname graphics
#' @export
theme_clean = function(base_size=12, base_family='sans') {
    ggplot2::theme(
        text= ggplot2::element_text(size=base_size, family=base_family),
        panel.background= ggplot2::element_rect(fill=NA),
        panel.border= ggplot2::element_rect(colour='grey50', fill=NA),
        panel.grid.major= ggplot2::element_line(colour='grey90', size=0.2),
        panel.grid.minor= ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill="grey90", colour="grey50", size=0.2),
        legend.key= ggplot2::element_rect(fill=NA))
}

#' Complete theme
#' @inheritParams ggplot2::ggtheme
#' @rdname graphics
#' @export
theme_wtl = function(base_size=12, base_family='sans') {
    ggplot2::theme_grey(base_size, base_family)+theme_clean(base_size, base_family)
}

#' Set L-shaped axes
#' @inheritParams ggplot2::element_line
#' @rdname graphics
#' @export
axis_line = function(colour=NULL, size=NULL, linetype=NULL, lineend=NULL) {
    el = ggplot2::element_line(colour=colour, size=size, linetype=linetype, lineend=lineend)
    ggplot2::theme(
        panel.border= ggplot2::element_blank(),
        axis.line.x= el, axis.line.y= el)  # workaround for ggplot2 2.1.0
}

#' Save PDF with quartz
#' @inheritParams ggplot2::ggsave
#' @param height see ggplot2::ggsave
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
