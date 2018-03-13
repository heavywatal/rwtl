#!/usr/bin/env Rscript --no-init-file
argv = commandArgs(trailingOnly = TRUE)
stopifnot(length(argv) > 0)
infile = argv[1]
outdir = argv[2]
if (is.na(outdir)) {
  outdir = getwd()
}
rmarkdown::render(infile, output_dir = outdir)
