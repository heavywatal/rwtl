#' Utilities for printing
#'
#' @examples
#' \dontrun{
#' printdf(mpg)
#' max_print(mpg)
#' }
#' @param x an object to print
#' @param n maximum number of rows to print
#' @param ... further arguments passed to `print`
#' @details
#' `printdf` is a simple cherry-picking from
#' `tibble:::print.tbl` and `data.frame:::print.data.table`.
#' @rdname print
#' @export
printdf <- function(x, n = getOption("tibble.print_max", 30L), ...) {
  cat("# ", class(x)[1L], ": ", nrow(x), " x ", ncol(x), "\n", sep = "")
  if (length(gvars <- dplyr::group_vars(x))) {
    gvars = paste0(gvars, collapse = ", ")
    cat("# ", dplyr::n_groups(x), " groups: ", gvars, "\n", sep = "")
  }
  if (ncol(x) == 0L) return(invisible(x))
  original_x = x
  class(x) = "data.frame" # remove tbl_df not to lose rownames when subsetting
  classes = vapply(x, class_sum, "", USE.NAMES = FALSE)
  if (n <= 0L) {
    x = x[FALSE,]
  }
  topn = ceiling(n / 2)
  if (nrow(x) > n) {
    x = rbind(utils::head(x, topn), utils::tail(x, n - topn))
    needs_separator = nrow(x) > 1L
  } else {
    needs_separator = FALSE
  }
  rn = rownames(x)
  x = do.call(cbind, lapply(x, format_column))
  # now x is a data.frame of formatted strings.
  rownames(x) = rn
  if (needs_separator) {
    x = rbind(utils::head(x, topn), "--" = "", utils::tail(x, n - topn))
  }
  x = rbind(" " = paste0("<", classes, ">"), x)
  rownames(x) = format(rownames(x), justify="right")
  print(x, right = TRUE, quote = FALSE, ...)
  invisible(original_x)
}

format_column = function(x) {
  if (is.list(x))
    vapply(x, format_list_item, "", USE.NAMES = FALSE)
  else if (is.character(x))
    trunc_chr(x)
  else
    format(x, justify = "none")
}

format_list_item = function(x) {
  if (is.null(x)) {
    character(0L)
  } else if (!is.null(dim(x))) {
    d = paste0(dim(x), collapse = " x ")
    paste0("<", class_sum(x), " [", d, "]>")
  } else if (is.atomic(x)) {
    paste0("<", class_sum(x), " [", length(x), "]>")
  } else if (inherits(x, "formula")) {
    trunc_chr(format(x))
  } else {
    paste0("<", class_sum(x), ">")
  }
}

class_sum = function(x) {
  switch(class(x)[1L],
    logical = "lgl",
    integer = "int",
    numeric = "dbl",
    character = "chr",
    complex = "cpl",
    factor = "fct",
    ordered = "ord",
    POSIXct = "dttm",
    Date = "date",
    class(x)[1L]
  )
}

trunc_chr <- function(x, n = 60L) {
  if (n > 0L) {
    idx = nchar(x) > n
    x[idx] = paste0(substr(x[idx], 1L, n), "...")
  }
  x
}

#' @details
#' `max_print` prints as many elements in a big tibble as possible.
#' @param width maximum number of columns to print
#' @rdname print
#' @export
max_print = function(x, n = getOption("max.print"), width = Inf, ...) {
  opts = options_print(height = n, width = width, max.print = n)
  on.exit(options(opts))
  print(x, ...)
}

#' @details
#' `adjust_print_options` sets width and height according to the current environment.
#' @rdname print
#' @export
adjust_print_options = function(n = 30L) {
  # COLUMNS and LINES are unreadable during startup
  stty_size = system("stty size", intern = TRUE)
  message("stty size: ", stty_size)
  stty_size = as.integer(strsplit(stty_size, " ")[[1L]])
  stopifnot(length(stty_size) > 1L)
  stopifnot(stty_size > 10L)
  options_print(
    height = min(stty_size[1L] - 6L, n),
    width = stty_size[2L]
  )
  if (interactive()) {
    message("datatable.print.nrows: ", getOption("datatable.print.nrows"))
    message("datatable.print.topn: ", getOption("datatable.print.topn"))
    message("tibble.print_max: ", getOption("tibble.print_max"))
    message("tibble.print_min: ", getOption("tibble.print_min"))
    message("width: ", getOption("width"))
  }
}

options_print = function(height, width, ...) {
  options(
    datatable.print.nrows = height,
    datatable.print.topn = height %/% 2L,
    tibble.print_max = height,
    tibble.print_min = height,
    tibble.width = width,
    width = min(width, 10000L),
    ...
  )
}
