#' Incomplete theme that does not overwrite other elements
#' @rdname theme
#' @export
theme_clean = function() {
    ggplot2::theme(
        panel.background= ggplot2::element_rect(fill=NA, colour=NA),
        panel.border= ggplot2::element_rect(fill=NA, colour='grey20'),
        panel.grid.major= ggplot2::element_line(colour='grey92'),
        panel.grid.minor= ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(fill="grey85", colour="grey20"),
        legend.key= ggplot2::element_blank(),
        axis.line= ggplot2::element_blank(),
        complete=TRUE)
}

#' Complete theme based on theme_bw()
#' @inheritParams ggplot2::theme_bw
#' @param base_size,base_family see ggplot2::ggtheme
#' @rdname theme
#' @export
theme_wtl = function(base_size=12, base_family='sans') {
    ggplot2::theme_bw(base_size, base_family) %>%
    ggplot2::`%+replace%`(theme_clean())
}

#' Set L-shaped axes
#' @inheritParams ggplot2::element_line
#' @rdname theme
#' @export
axis_line = function(colour=NULL, size=NULL, linetype=NULL, lineend=NULL, arrow=NULL, inherit.blank=FALSE) {
    el = ggplot2::element_line(colour=colour, size=size, linetype=linetype, lineend=lineend, arrow=arrow, inherit.blank=inherit.blank)
    ggplot2::theme(panel.border= ggplot2::element_blank(), axis.line= el)
}
