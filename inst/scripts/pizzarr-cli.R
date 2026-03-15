#!/usr/bin/env Rscript

# Minimal CLI for zarr-conformance-tests (Bisaloo/zarr-conformance-tests).
# Reads a Zarr array at the given path and exits 0 on success, 1 on failure.
#
# Usage:
#   Rscript pizzarr-cli.R --array_path=/path/to/array.zarr

args <- commandArgs(trailingOnly = TRUE)

# Simple --key=value parser
opts <- grep("^--", args, value = TRUE)
parsed <- strsplit(opts, "=")
opt_list <- setNames(
  vapply(parsed, function(x) paste(x[-1], collapse = "="), ""),
  vapply(parsed, function(x) sub("^--", "", x[1]), "")
)

if (!"array_path" %in% names(opt_list)) {
  cat("Error: --array_path is required\n", file = stderr())
  quit(status = 1)
}

array_path <- opt_list[["array_path"]]

library(pizzarr)

arr <- zarr_open(array_path, mode = "r")
data <- arr$get_item("...")$data
cat(sprintf("OK: read array at %s (%d elements)\n", array_path, length(data)))
