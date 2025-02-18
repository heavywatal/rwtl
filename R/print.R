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
#' @param classes print class of each column if TRUE
#' @param ... further arguments passed to `print`
#' @description
#' `printdf()` is a simple cherry-picking from
#' [tibble::print.tbl()] and `data.frame:::print.data.table`.
#' @rdname print
#' @export
printdf = function(x, n = getOption("pillar.print_max", 30L),
                   summarize = getOption("wtl.printdf.summarize", TRUE),
                   classes = getOption("wtl.printdf.classes", TRUE), ...) {
  if (isTRUE(summarize)) printdf_summary(x)
  if (is.null(dim(x))) {
    return(print(x))
  }
  if (ncol(x) == 0L) {
    return(invisible(x))
  }
  if (is.matrix(x)) x = as.data.frame(x)
  nrow_x = nrow(x)
  original_x = x
  class(x) = "data.frame" # remove tbl_df
  x = dedfcol_all(x)
  x = demtrxcol_all(x)
  truncated = nrow_x > n
  y = x
  if (truncated) {
    head_n = as.integer(ceiling(n / 2))
    tail_n = n - head_n
    head_idx = seq_len(head_n)
    tail_idx = seq.int(to = nrow_x, length.out = tail_n)
    y = y[c(head_idx, tail_idx), , drop = FALSE]
    # subsetting changes class <ts>
  }
  y = do.call(cbind, lapply(y, format_column))
  # now x is a data.frame of formatted strings.
  if (truncated) {
    rnames = c(head_idx, "--", tail_idx)
    tail_idx = seq.int(to = n, length.out = tail_n)
    y = rbind(y[head_idx, , drop = FALSE], "", y[tail_idx, , drop = FALSE])
  } else {
    rnames = seq_len(nrow_x)
  }
  if (isTRUE(classes)) {
    class_row = vapply(x, class_sum, "", USE.NAMES = FALSE)
    class_row = paste0("<", class_row, ">")
    y = rbind(class_row, y)
    rnames = c("", rnames)
  }
  row.names(y) = format(rnames, justify = "right")
  print(y, right = TRUE, quote = FALSE, max = .Machine$integer.max, ...)
  invisible(original_x)
}

format_column = function(x) {
  if (is.list(x)) {
    if (inherits(x, "bench_expr")) {
      keys = names(x)
      values = vapply(x, deparse, "", width.cutoff = 500L, USE.NAMES = FALSE)
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
  d = paste(DIM(x), collapse = " x ")
  paste0(class_sum(x), " [", d, "]")
}

class_sum = function(x) {
  switch(class(x)[[1L]],
    logical = "lgl",
    integer = "int",
    numeric = "dbl",
    character = "chr",
    complex = "cpl",
    factor = "fct",
    ordered = "ord",
    POSIXct = "dttm",
    Date = "date",
    class(x)[[1L]]
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
    gvars = toString(dplyr::group_vars(x))
    cat("# Groups: ", gvars, " [", dplyr::n_groups(x), "]\n", sep = "")
  }
}

#' @description
#' `max_print()` prints as many elements in a big tibble as possible.
#' @param width maximum number of columns to print
#' @rdname print
#' @export
max_print = function(x, n = .Machine$integer.max, width = Inf, ...) {
  opts = propagate_print_options(height = n, width = width, max.print = n)
  withr::with_options(opts, {
    print(x, ...)
  })
}

#' @description
#' `generate_print_options()` translates `n` and stty size to a list of options.
#' @rdname print
#' @export
generate_print_options = function(n = 30L) {
  # During startup:
  # - COLUMNS and LINES are not available
  # - options("width") is fixed to 80
  console_size = tryCatch(
    stty_size(),
    error = as.null,
    warning = as.null
  )
  if (length(console_size) == 2L) {
    propagate_print_options(
      height = min(console_size[[1L]] - 6L, n),
      width = console_size[[2L]]
    )
  } else {
    propagate_print_options(height = n)
  }
}

#' @description
#' `show_print_options()` prints the current values of print options.
#' @rdname print
#' @export
show_print_options = function() {
  opts = propagate_print_options() |>
    names() |>
    getOptions()
  utils::str(opts, no.list = TRUE)
  invisible(opts)
}

stty_size = function() {
  stty = system2("stty", "size", stdout = TRUE, stderr = FALSE)
  as.integer(unlist(strsplit(stty, " ", fixed = TRUE)))
}

propagate_print_options = function(height = 20L, width = NULL, ...) {
  list(
    datatable.print.nrows = height,
    datatable.print.topn = height %/% 2L,
    pillar.print_max = height,
    pillar.print_min = height,
    pillar.width = width,
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

getOptions = function(x) {
  rlang::set_names(x) |> lapply(getOption)
}
