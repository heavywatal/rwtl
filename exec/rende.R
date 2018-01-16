#!/usr/bin/env Rscript
argv = commandArgs(trailingOnly = TRUE)
infile = argv[1]
outdir = argv[2]
if (is.na(outdir)) {
  outdir = getwd()
}
message("infile: ", infile)
message("outdir: ", outdir)
rmarkdown::render(infile, output_dir = outdir)
