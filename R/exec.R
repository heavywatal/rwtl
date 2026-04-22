#' Shortcut to use knitr and rmarkdown from commandline
#'
#' `exec_knit()` temporarily modifies the environment before knitting:
#' - Set the working directory to the output directory as recommended by knitr.
#' - Set `unnamed.chunk.label` and `cache.path` using the input file name to avoid conflicts.
#' - Set `error = FALSE`, `message = NA`, and `warning = NA` for easier debugging.
#' - Customize `knitr.progress.fun` for better progress display.
#' - Enable caching for the Stan engine.
#' @param input File path.
#' @param output File or directory path.
#' @returns `exec_knit()` returns the output from `knitr::knit()`.
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
      outdir = dirname(output)
      fs::dir_create(outdir)
      withr::local_dir(outdir)
      output = basename(output)
    }
  }
  inputname = stringr::str_replace_all(basename(input), "\\W+", "-")
  knitr::opts_knit$set(unnamed.chunk.label = glue("_{inputname}"))
  knitr::opts_chunk$set(cache.path = glue(".cache/{inputname}/"))
  knitr::opts_chunk$set(error = FALSE)
  knitr::opts_chunk$set(message = NA)
  knitr::opts_chunk$set(warning = NA)
  withr::local_options(knitr.progress.fun = knitr_progress)
  knit_engines_set_cache_stan()
  invisible(knitr::knit(input, output))
}

#' @param output_dir Directory path.
#' @returns `exec_render()` returns the output from `rmarkdown::render()`.
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
    update = \(i) {
      if (nzchar(labels[i])) {
        cli::cli_progress_step(labels[i], .envir = envir)
      }
    },
    done = \() cli::cli_process_done(.envir = envir)
  )
}
