# Convert bcsd_v2.zarr.zip (V2) to bcsd_v3.zarr.zip (V3) using pizzarr.
#
# R equivalent of convert-bcsd-v3.py.
# Unzips the V2 store, reads each array and group,
# and writes an equivalent Zarr V3 store, then zips it.
#
# Usage:
#   Rscript convert-bcsd-v3.R

library(pizzarr)

get_script_dir <- function() {
  # Works with Rscript --file, source(), and commandArgs
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  # Fallback: assume working directory contains inst/extdata
  return(normalizePath("inst/extdata"))
}
script_dir <- get_script_dir()

v2_zip <- file.path(script_dir, "bcsd_v2.zarr.zip")
v3_zip <- file.path(script_dir, "bcsd_v3.zarr.zip")

stopifnot(file.exists(v2_zip))

# Clean up previous output
if (file.exists(v3_zip)) file.remove(v3_zip)

# Work in temp directories
tmpdir <- tempfile("bcsd_convert")
dir.create(tmpdir)
on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

v2_extract <- file.path(tmpdir, "v2")
v3_dir <- file.path(tmpdir, "bcsd_v3.zarr")

# Unzip V2 store
utils::unzip(v2_zip, exdir = v2_extract)
v2_path <- file.path(v2_extract, "bcsd_v2.zarr")
cat("V2 store extracted to:", v2_path, "\n")

# Open V2 root group
v2_root <- zarr_open_group(v2_path, mode = "r")

# Create V3 output store
dir.create(v3_dir)
v3_store <- DirectoryStore$new(v3_dir)
v3_root <- zarr_create_group(store = v3_store, zarr_format = 3L)

# Copy root-level attributes
root_attrs <- v2_root$get_attrs()$to_list()
if (length(root_attrs) > 0) {
  v3_attrs <- v3_root$get_attrs()
  for (nm in names(root_attrs)) {
    v3_attrs$set_item(nm, root_attrs[[nm]])
  }
  cat("Root attributes:", length(root_attrs), "keys copied\n")
}

# Known arrays in bcsd store
array_names <- c("latitude", "longitude", "time", "pr", "tas")

for (name in array_names) {
  v2_arr <- v2_root$get_item(name)
  data <- v2_arr$as.array()
  shape <- v2_arr$get_shape()
  chunks <- v2_arr$get_chunks()
  dtype <- v2_arr$get_dtype()
  fill_value <- v2_arr$get_fill_value()
  compressor <- v2_arr$get_compressor()

  cat(sprintf("  Array '%s': shape=(%s), dtype=%s, chunks=(%s)\n",
              name,
              paste(shape, collapse = ", "),
              dtype$dtype,
              paste(chunks, collapse = ", ")))

  # Create V3 array (zarr_format inherited from parent group)
  v3_arr <- v3_root$create_dataset(
    name = name,
    shape = as.integer(shape),
    chunks = as.integer(chunks),
    dtype = dtype$dtype,
    fill_value = fill_value,
    compressor = compressor
  )

  # Write data
  v3_arr$set_item("...", data)

  # Copy array attributes
  arr_attrs <- v2_arr$get_attrs()$to_list()
  if (length(arr_attrs) > 0) {
    v3_a <- v3_arr$get_attrs()
    for (nm in names(arr_attrs)) {
      v3_a$set_item(nm, arr_attrs[[nm]])
    }
    cat("    Attributes:", paste(names(arr_attrs), collapse = ", "), "\n")
  }
}

cat("\nV3 store written to:", v3_dir, "\n")

# Verify by reading back
cat("\nVerifying V3 store...\n")
v3_check_store <- DirectoryStore$new(v3_dir)
v3_check <- zarr_open_group(v3_check_store, mode = "r")

for (name in array_names) {
  arr <- v3_check$get_item(name)
  v2_arr <- v2_root$get_item(name)

  v3_data <- arr$as.array()
  v2_data <- v2_arr$as.array()

  shape_str <- paste(arr$get_shape(), collapse = ", ")
  match <- identical(dim(v3_data), dim(v2_data)) && all(v3_data == v2_data, na.rm = TRUE)
  cat(sprintf("  Verify '%s': shape=(%s), data_match=%s\n", name, shape_str, match))
}

# Zip it up
cat(sprintf("\nCreating %s...\n", v3_zip))
old_wd <- setwd(dirname(v3_dir))
zip(v3_zip, files = basename(v3_dir))
setwd(old_wd)

cat("Done!\n")
