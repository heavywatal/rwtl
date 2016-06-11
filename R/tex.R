#' Print power of ten
#' @param fmt printf string
#' @param n numeric
#' @return string
#' @rdname tex
#' @export
powerintf = function(fmt, n) {
    powerintf_ = function(n) {
        exponent = exponent(n, 10)
        mantissa = mantissa(n, 10, exponent)
        sprintf("%.3f $\\times$ 10\\textsuperscript{%.0f}", mantissa, exponent)
    }
    s = sprintf(fmt, n)
    ifelse(grepl("e", s), powerintf_(n), s)
}

#' Translate data.frame to LaTeX table
#' @param x data.frame
#' @param file string
#' @param col.names logical
#' @rdname tex
#' @export
write_tabular = function(x, file='', col.names=TRUE) {
    if ("Estimate" %in% names(x)) {
        x$"Estimate" = powerintf("%.3g", x$"Estimate")
        x$"Estimate" = sub("(.+)", "\\1", x$"Estimate")
    }
    if ("Adjusted $\\alpha$" %in% names(x)) {
        x$"Adjusted $\\alpha$" = powerintf("%.3g", x$"Adjusted $\\alpha$")
        x$"Adjusted $\\alpha$" = sub("(.+)", "\\1", x$"Adjusted $\\alpha$")
    }
    if ("Observed P" %in% names(x)) {
        x$"Observed P" = powerintf("%.3g", x$"Observed P")
        x$"Observed P" = ifelse(x$Significance, sprintf("{%s}\\textsuperscript{*}", x$"Observed P"), x$"Observed P")
        x$"Observed P" = sub("(.+)", "\\1", x$"Observed P")
        x = x[, setdiff(colnames(x), "Significance")]
    }
    if (col.names) {
        utils::write.table(x[0, ], file=file, quote=FALSE, sep=" & ", eol=" \\\\ \\hline\n",
            row.names=FALSE, col.names=TRUE)
    }
    utils::write.table(x, file=file, quote=FALSE, sep=" & ", eol=" \\\\\n",
        row.names=FALSE, col.names=FALSE, append=TRUE)
}
