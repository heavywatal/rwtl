#!/usr/bin/env Rscript --no-init-file
argv = commandArgs(trailingOnly = TRUE)
stopifnot(length(argv) > 0L)
wtl::DiagrammeR_dot2svg(argv[1L], argv[2L])
