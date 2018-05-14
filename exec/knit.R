#!/usr/bin/env Rscript --no-init-file
argv = commandArgs(trailingOnly = TRUE)
stopifnot(length(argv) > 0L)
output = argv[2L]
if (!is.na(output)) {
  outdir = dirname(normalizePath(output))
  knitr::opts_knit$set(base.dir = outdir)
}
knitr::knit(argv[1L], output)
