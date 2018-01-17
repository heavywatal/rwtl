#!/usr/bin/env Rscript
argv = commandArgs(trailingOnly = TRUE)
stopifnot(length(argv) > 0)
knitr::knit(argv[1])
