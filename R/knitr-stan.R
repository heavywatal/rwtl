#' Register knitr engine for stan code
#'
#' This is a simple alternative to [cmdstanr::register_knitr_engine()].
#' Chunk code is not compiled, but just written to a file with decent caching
#' to facilitate showing the usage of [cmdstanr::cmdstan_model()].
#'
#' @details
#' Stan file is automatically named after the chunk label, .i.e.,
#' `"{cache_stan_prefix}{label}.stan"`.
#'
#' Set `cache_stan` to `TRUE` or `FALSE` for stan chunks so that
#' `output.var` can be omitted. Then set it to the chunk label of stan code
#' for dependent R chunks.
#'
#' @param cache_stan_prefix Prefix for output files.
#' A relative path is evaluated from the source file.
#' @rdname knitr-stan
#' @export
knit_engines_set_cache_stan = function(cache_stan_prefix = NULL) {
  knitr::knit_engines$set(stan = eng_cache_stan)
  knitr::opts_chunk$set(cache_stan_prefix = cache_stan_prefix)
  knitr::opts_hooks$set(cache_stan = hook_cache_stan)
  knitr::knit_hooks$set(stan_save_output_files = hook_save_output_files)
  # to suppress "warning: jobserver unavailable: using -j1" from cmdstanr
  withr::local_envvar(MAKEFLAGS = "-j1", .local_envir = knitr_chunk_envir())
}

# @param opts options.
# Renamed to work around "undesirable" false positives from lintr.
eng_cache_stan = function(opts) {
  stopifnot(length(opts$output.var) == 1L)
  stan_file = stan_file_cache(opts)
  if (!file.exists(stan_file) || any(opts$code != readLines(stan_file))) {
    dir.create(dirname(stan_file), showWarnings = FALSE, recursive = FALSE, mode = "0755")
    cat(opts$code, file = stan_file, sep = "\n")
  } else {
    message(stan_file, " is up to date.")
  }
  opts$engine = "stan"
  assign(opts$output.var, tools::md5sum(stan_file), envir = knitr_chunk_envir())
  knitr::engine_output(opts, opts$code, "")
}

# Workaround to achieve the following benefits at the same time.
#
# engine: stan (for code highlighting)
# cache: true
# output.var: null (automatically named after `label`)
#
# The engine name "stan" requires `output.var` to be set for cache invalidation,
# which happens before an engine function.
# Option hooks seem to be the only way to go for now.
# See `knitr:::block_exec()` in `block.R`.
hook_cache_stan = function(opts) {
  if (is.character(opts$cache_stan)) {
    # {r} with cmdstan_model()
    objname = paste0(".md5_", opts$cache_stan)
    opts$cache.extra = paste0(get(objname, envir = knitr_chunk_envir()))
    opts$cache = TRUE
    opts$message = NA
    opts$warning = NA
  } else {
    # {stan}
    stan_file = stan_file_cache(opts)
    opts$cache = as.logical(opts$cache_stan)
    opts$cache.rebuild = opts$cache && !file.exists(stan_file)
    opts$output.var = paste0(".md5_", opts$label)
  }
  opts
}

# Cached `fit` objects are usually not usable in other R sessions because they
# only store paths to CSV files, which are written to `tempdir()` by default.
# This hook moves files to a better place and sets internal paths to them.
hook_save_output_files = function(before, options, envir) {
  if (!before) {
    outdir = file.path(options$cache.path, "stan")
    fs::dir_create(outdir)
    objname = options$stan_save_output_files
    fit = get(objname, envir = knitr_chunk_envir())
    new_path = suppressMessages(
      fit$save_output_files(outdir, timestamp = FALSE, random = FALSE)
    )
    message("save_output_files: ", new_path[1])
  }
  invisible()
}

stan_file_cache = function(opts) {
  prefix = opts$cache_stan_prefix %||% ""
  if (!fs::is_absolute_path(prefix)) {
    prefix = file.path(dirname(knitr::current_input(dir = TRUE)), prefix)
  }
  paste0(prefix, opts$label, ".stan")
}

# workaround for "assignments to the global environment"
knitr_chunk_envir = function() knitr::knit_global()
