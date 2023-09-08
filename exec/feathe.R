#!/usr/bin/env Rscript --no-init-file
.argv = commandArgs(trailingOnly = TRUE)
.tbl = arrow::read_feather(.argv[1])
readr::write_tsv(.tbl, stdout())
