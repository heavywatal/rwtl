#!/usr/bin/env Rscript --no-init-file
argv = commandArgs(trailingOnly = TRUE)
stopifnot(length(argv) > 0L)
output = argv[2L]
if (!is.na(output)) {
  fig.path = paste0(dirname(output), "/figure/")
  knitr::opts_chunk$set(fig.path = fig.path)
}
knitr::knit(argv[1L], output)
