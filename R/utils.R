#' Utility functions
#'
#' @details
#' `easierprof` is a simple wrapper of `Rprof()` and `summaryRprof()`.
#' @param expr R expression
#' @param interval numeric
#' @param memory logical
#' @rdname utils
#' @export
easierprof = function(expr, interval = 0.02, memory = FALSE) {
  .tmpfile = tempfile()
  utils::Rprof(.tmpfile, interval = interval, memory.profiling = memory)
  eval(substitute(expr))
  utils::Rprof(NULL)
  utils::summaryRprof(.tmpfile, memory = ifelse(memory, "both", "none"))
}

#' @details
#' `getenv` converts a result of `Sys.getenv()` to data.frame.
#' @param pattern R expression
#' @rdname utils
#' @export
getenv = function(pattern = NULL) {
  env = Sys.getenv()
  class(env) = "character"
  env = tibble::tibble(key = names(env), value = env)
  if (!is.null(pattern)) {
    env = dplyr::filter(env, stringr::str_detect(.data$key, pattern))
  }
  env
}

#' @details
#' `reprex_tidyverse` wraps [reprex::reprex]
#' @inheritParams printdf
#' @param venue character; see [reprex::reprex]
#' @param show logical
#' @rdname utils
#' @export
reprex_tidyverse = function(n = 8L, venue = "r", show = FALSE) {
  reprex::reprex(input = c(
    "library(tidyverse)",
    "registerS3method(\"print\", \"tbl_df\", wtl::printdf)",
    sprintf("options(pillar.print_max = %dL)", n),
    "",
    clipr::read_clip() |> stringr::str_subset("^#>", negate = TRUE)
  ), venue = venue, show = show)
  clipr::read_clip() |>
    utils::tail(-4L) |>
    clipr::write_clip()
}

clean_histfile = function(dry_run = FALSE) {
  histfile = tempfile(fileext = ".Rhistory")
  cat(histfile, "\n")
  utils::savehistory(histfile)
  orig = readLines(histfile)
  res = orig |>
    stringr::str_subset("^\\s*#", negate = TRUE) |>
    unique(fromLast = TRUE)
  cat(length(orig), "->", length(res), "lines\n")
  writeLines(res, histfile)
  if (!dry_run) {
    utils::loadhistory(histfile)
    cat("`quit(runLast = FALSE)` to discard a new history.\n")
  }
}
