#!/usr/bin/env Rscript --no-init-file
argv = commandArgs(trailingOnly = TRUE)
stopifnot(length(argv) > 0)
knitr::knit(argv[1])
