#' Shortcut to use knitr and rmarkdown from commandline
#'
#' @param input File path.
#' @param output File or directory path.
#' @rdname exec
#' @export
exec_knit = function(input, output = NA) {
  if (is.na(output)) {
    withr::local_dir(dirname(input))
    input = basename(input)
    output = NULL
  } else {
    input = normalizePath(input)
    if (dir.exists(output)) {
      withr::local_dir(output)
      output = NULL
    } else {
      withr::local_dir(dirname(output))
      output = basename(output)
    }
  }
  inputname = stringr::str_replace_all(basename(input), "\\W+", "-")
  knitr::opts_knit$set(unnamed.chunk.label = glue("_{inputname}"))
  knitr::opts_chunk$set(cache.path = glue(".cache/{inputname}/"))
  knitr::opts_chunk$set(error = FALSE)
  withr::local_options(knitr.progress.fun = knitr_progress)
  knit_engines_set_cache_stan()
  invisible(knitr::knit(input, output))
}

#' @param output_dir Directory path.
#' @rdname exec
#' @export
exec_render = function(input, output_dir = NA) {
  if (is.na(output_dir)) {
    output_dir = getwd()
  }
  rmarkdown::render(input, output_dir = output_dir)
}

knitr_progress = function(total, labels) {
  envir = parent.frame()
  list(
    update = \(i) if (nzchar(labels[i])) {
      cli::cli_progress_step(labels[i], .envir = envir)
    },
    done = \() cli::cli_process_done(.envir = envir)
  )
}
