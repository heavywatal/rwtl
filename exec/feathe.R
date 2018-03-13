#!/usr/bin/env Rscript --no-init-file
.argv = commandArgs(trailingOnly = TRUE)
.tbl = feather::read_feather(.argv[1])
readr::write_tsv(.tbl, "/dev/stdout")
