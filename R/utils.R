#' Utility functions
#'
#' @description
#' `easierprof()` is a simple wrapper of [utils::Rprof()] and [utils::summaryRprof()].
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

#' @description
#' `getenv()` converts a result of [Sys.getenv()] to data.frame.
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

#' @description
#' `reprex_tidyverse()` wraps [reprex::reprex()]
#' @param n passed to `pillar.print_max`
#' @inheritParams reprex::reprex
#' @rdname utils
#' @export
reprex_tidyverse = function(venue = "r", n = 8L, html_preview = FALSE) {
  setup = c(
    "library(tidyverse)",
    "registerS3method(\"print\", \"tbl_df\", wtl::printdf)",
    sprintf("options(pillar.print_max = %dL)", n)
  )
  code = clipr::read_clip() |> stringr::str_subset("^#>", negate = TRUE)
  input = c(
    "#+ setup, include = FALSE",
    setup,
    "#+ actual-reprex-code",
    code
  )
  reprex::reprex(input = input, venue = venue, html_preview = html_preview)
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
