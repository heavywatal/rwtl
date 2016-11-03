#' Incomplete theme that does not overwrite other elements
#' @inheritParams ggplot2::ggtheme
#' @rdname theme
#' @export
theme_clean = function(base_size=12, base_family='sans') {
    ggplot2::theme(
        text= ggplot2::element_text(size=base_size, family=base_family),
        panel.background= ggplot2::element_rect(fill=NA),
        panel.border= ggplot2::element_rect(colour='grey50', fill=NA),
        panel.grid.major= ggplot2::element_line(colour='grey90', size=0.2),
        panel.grid.minor= ggplot2::element_blank(),
        axis.line.x= ggplot2::element_blank(),
        axis.line.y= ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill="grey90", colour="grey50", size=0.2),
        legend.key= ggplot2::element_blank())
}

#' Complete theme based on theme_bw()
#' @inheritParams ggplot2::ggtheme
#' @rdname theme
#' @export
theme_wtl = function(base_size=12, base_family='sans') {
    ggplot2::theme_bw(base_size, base_family)+theme_clean(base_size, base_family)
}

#' Set L-shaped axes
#' @inheritParams ggplot2::element_line
#' @rdname theme
#' @export
axis_line = function(colour=NULL, size=NULL, linetype=NULL, lineend=NULL) {
    el = ggplot2::element_line(colour=colour, size=size, linetype=linetype, lineend=lineend)
    ggplot2::theme(
        panel.border= ggplot2::element_blank(),
        axis.line= el, axis.line.x= el, axis.line.y= el)  # workaround for ggplot2 2.1.0
}
