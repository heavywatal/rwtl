#' Shortcut for grid::viewport()
#' @param row layout.pos.row
#' @param col layout.pos.col
#' @return grid::viewport
#' @rdname grid
#' @export
viewport_layout = function(row, col) {
    grid::viewport(layout.pos.row=row, layout.pos.col=col)
}

#' Just another gridExtra::arrangeGrob()
#' @param grobs list of grobs
#' @param nrow,ncol integer
#' @param byrow logical
#' @return grid::gTree
#' @rdname grid
#' @export
grid_grob = function(grobs, nrow=NULL, ncol=NULL, byrow=FALSE) {
    n = length(grobs)
    if (is.null(nrow) & is.null(ncol)) {nrow = floor(n/2); ncol = ceiling(n/nrow)}
    if (is.null(nrow)) {nrow = ceiling(n/ncol)}
    if (is.null(ncol)) {ncol = ceiling(n/nrow)}
    fg = grid::frameGrob(layout=grid::grid.layout(nrow, ncol))
    if (byrow) {
        row = ceiling(seq(n) / ncol)
        col = ((seq(n) - 1) %% ncol + 1)
    } else {
        row = ((seq(n) - 1) %% nrow + 1)
        col = ceiling(seq(n) / nrow)
    }
    for (i in seq(n)) {
        .class = class(grobs[[i]])
        .grob = if ("ggplot" %in% .class) ggplot2::ggplotGrob(grobs[[i]])
                else grobs[[i]]
        fg = grid::placeGrob(fg, .grob, row=row[i], col=col[i])
    }
    invisible(grid::gTree(children=grid::gList(fg), cl="arrange"))
}

#' Shortcut for grid::grid.draw()
#' @inheritParams grid::grid.draw
#' @inheritParams base::print
#' @rdname grid
#' @export
print.grob = function(x, ...) grid::grid.draw(x, ...)
