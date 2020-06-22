#' Utilities for printing
#'
#' @examples
#' \dontrun{
#' printdf(mpg)
#' max_print(mpg)
#' }
#' @param x an object to print
#' @param n maximum number of rows to print
#' @param summarize print class(x) and dim(x) if TRUE
#' @param ... further arguments passed to `print`
#' @details
#' `printdf` is a simple cherry-picking from
#' `tibble:::print.tbl` and `data.frame:::print.data.table`.
#' @rdname print
#' @export
printdf = function(x, n = getOption("tibble.print_max", 30L), summarize = getOption("wtl.printdf.summarize", TRUE), ...) {
  if (isTRUE(summarize)) printdf_summary(x)
  if (is.null(dim(x))) {
    return(print(x))
  }
  if (ncol(x) == 0L) {
    return(invisible(x))
  }
  if (is.matrix(x)) x = as.data.frame(x)
  original_x = x
  class(x) = "data.frame" # remove tbl_df
  x = dedfcol_all(x)
  class_row = vapply(x, class_sum, "", USE.NAMES = FALSE)
  class_row = paste0("<", class_row, ">")
  truncated = nrow(x) > n
  if (truncated) {
    head_n = as.integer(ceiling(n / 2))
    tail_n = n - head_n
    head_idx = seq_len(head_n)
    tail_idx = seq.int(to = nrow(x), length.out = tail_n)
    x = x[c(head_idx, tail_idx), , drop = FALSE]
  }
  x = do.call(cbind, lapply(x, format_column))
  # now x is a data.frame of formatted strings.
  if (truncated) {
    rnames = c("", head_idx, "--", tail_idx)
    tail_idx = seq.int(to = nrow(x), length.out = tail_n)
    x = rbind(class_row, x[head_idx, , drop = FALSE], "", x[tail_idx, , drop = FALSE])
  } else {
    rnames = c("", seq_len(nrow(x)))
    x = rbind(class_row, x)
  }
  row.names(x) = format(rnames, justify = "right")
  print(x, right = TRUE, quote = FALSE, ...)
  invisible(original_x)
}

format_column = function(x) {
  if (is.list(x)) {
    if (inherits(x, "bench_expr")) {
      keys = names(x)
      values = vapply(x, deparse, "", USE.NAMES = FALSE)
      trunc_chr(ifelse(keys == "", values, keys), 32L)
    } else {
      vapply(x, format_list_item, "", USE.NAMES = FALSE)
    }
  } else {
    if (stats::is.ts(x)) {
      x = as.vector(x)
    }
    if (is.character(x)) {
      trunc_chr(x)
    } else {
      format(x, justify = "none")
    }
  }
}

format_list_item = function(x) {
  if (is.null(x)) {
    "<NULL>"
  } else if (is.list(x) || is.atomic(x)) {
    paste0("<", array_sum(x), ">")
  } else if (inherits(x, "formula")) {
    trunc_chr(format(x))
  } else {
    paste0("<", class_sum(x), ">")
  }
}

array_sum = function(x) {
  d = paste0(DIM(x), collapse = " x ")
  paste0(class_sum(x), " [", d, "]")
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

DIM = function(x) {
  d = dim(x)
  if (length(d)) d else length(x)
}

trunc_chr = function(x, n = 60L) {
  if (n > 0L) {
    idx = nchar(x, keepNA = FALSE) > n
    x[idx] = paste0(substr(x[idx], 1L, n), "...")
  }
  x
}

printdf_summary = function(x) {
  cat("# ", array_sum(x), "\n", sep = "")
  if (dplyr::is_grouped_df(x)) {
    gvars = paste0(dplyr::group_vars(x), collapse = ", ")
    cat("# Groups: ", gvars, " [", dplyr::n_groups(x), "]\n", sep = "")
  }
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
  # During startup:
  # - COLUMNS and LINES are not available
  # - options("width") is fixed to 80
  console_size = tryCatch(
    stty_size(),
    error = as.null,
    warning = as.null
  )
  opts = if (length(console_size) == 2L) {
    options_print(
      height = min(console_size[1L] - 6L, n),
      width = console_size[2L]
    )
  } else {
    options_print(height = n)
  }
  if (interactive()) {
    message("datatable.print.nrows: ", getOption("datatable.print.nrows"))
    message("datatable.print.topn: ", getOption("datatable.print.topn"))
    message("tibble.print_max: ", getOption("tibble.print_max"))
    message("tibble.print_min: ", getOption("tibble.print_min"))
    message("width: ", getOption("width"))
  }
  invisible(opts)
}

stty_size = function() {
  as.integer(unlist(strsplit(system2("stty", "size", stdout = TRUE, stderr = FALSE), " ")))
}

options_print = function(height, width = NULL, ...) {
  options(
    datatable.print.nrows = height,
    datatable.print.topn = height %/% 2L,
    tibble.print_max = height,
    tibble.print_min = height,
    tibble.width = width,
    width = sanitize_width(width),
    ...
  )
}

sanitize_width = function(x) {
  if (is.numeric(x)) {
    max(10L, min(x, 10000L))
  } else {
    getOption("width")
  }
}
