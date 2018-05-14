#!/usr/bin/env Rscript --no-init-file
argv = commandArgs(trailingOnly = TRUE)
stopifnot(length(argv) > 0L)
input = argv[1L]
output = argv[2L]
if (!is.na(output)) {
  output = normalizePath(output)
  knitr::opts_knit$set(base.dir = dirname(output))
}
setwd(dirname(input))
knitr::knit(basename(input), output)
