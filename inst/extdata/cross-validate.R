# Cross-validate pizzarr (R) and zarr-python produce equivalent Zarr stores.
#
# Writes identical arrays from both R and Python (V2 + V3), then each
# implementation reads the other's output.  Prints a pass/fail summary
# and exits with code 0 (all pass) or 1 (any fail).
#
# Prerequisites: Python 3.10+ with zarr>=3 and numpy installed.
# Skips gracefully if Python is not available.
#
# Usage:
#   Rscript inst/extdata/cross-validate.R

# Use devtools::load_all() when running from the source tree, otherwise
# fall back to the installed package.
if (file.exists("DESCRIPTION") &&
any(grepl("^Package: pizzarr", readLines("DESCRIPTION", n = 5)))) {
  devtools::load_all(".")
} else {
  library(pizzarr)
}

# ---------------------------------------------------------------------------
# 0. Python detection
# ---------------------------------------------------------------------------
python_cmd <- Sys.which("python")
if (nchar(python_cmd) == 0) python_cmd <- Sys.which("python3")
if (nchar(python_cmd) == 0) {
  message("SKIP: Python not found on PATH.")
  quit(status = 0)
}

zarr_ver <- tryCatch(
  system2(python_cmd, c("-c", shQuote("import zarr; print(zarr.__version__)")),
  stdout = TRUE, stderr = TRUE),
  error = function(e) NULL
)
if (is.null(zarr_ver) || !is.null(attr(zarr_ver, "status"))) {
  message("SKIP: zarr-python not installed. Install via: pip install 'zarr>=3' numpy")
  quit(status = 0)
}
cat(sprintf("Python zarr %s detected (%s)\n", zarr_ver, python_cmd))

# Locate companion Python scripts (next to this script in inst/extdata)
script_dir <- if (file.exists("inst/extdata/cross-validate-write.py")) {
  normalizePath("inst/extdata", winslash = "/")
} else {
  normalizePath(system.file("extdata", package = "pizzarr"), winslash = "/")
}
py_write_script <- file.path(script_dir, "cross-validate-write.py")
py_read_script  <- file.path(script_dir, "cross-validate-read.py")

if (!file.exists(py_write_script) || !file.exists(py_read_script)) {
  stop("Cannot find cross-validate-write.py and cross-validate-read.py in ", script_dir)
}

# ---------------------------------------------------------------------------
# 1. Temp working directory
# ---------------------------------------------------------------------------
work_dir <- tempfile("pizzarr_crossval")
dir.create(work_dir)
on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
cat("Working directory:", work_dir, "\n")

# ---------------------------------------------------------------------------
# 2. Test case definitions (shared contract between R and Python)
# ---------------------------------------------------------------------------
test_cases <- list(
  list(
    name       = "int32_1d_gzip",
    data       = c(10L, 20L, 30L, 40L, 50L, 60L),
    shape      = 6L,
    chunks     = 3L,
    dtype_r    = "<i4",
    codec      = "gzip",
    fill_value = 0L
  ),
  list(
    name       = "float64_1d_zstd",
    data       = c(1.1, 2.2, 3.3, 4.4, 5.5),
    shape      = 5L,
    chunks     = 5L,
    dtype_r    = "<f8",
    codec      = "zstd",
    fill_value = 0
  ),
  list(
    name       = "bool_1d_none",
    data       = c(TRUE, FALSE, TRUE, FALSE),
    shape      = 4L,
    chunks     = 4L,
    dtype_r    = "|b1",
    codec      = "none",
    fill_value = FALSE
  ),
  list(
    name       = "uint8_1d_none",
    data       = as.integer(c(0, 127, 128, 255)),
    shape      = 4L,
    chunks     = 2L,
    dtype_r    = "|u1",
    codec      = "none",
    fill_value = 0L
  ),
  list(
    name       = "float32_1d_gzip",
    data       = c(-1.5, 0.0, 1.5, 3.140000104904175),
    shape      = 4L,
    chunks     = 4L,
    dtype_r    = "<f4",
    codec      = "gzip",
    fill_value = 0
  ),
  list(
    name       = "int16_2d_gzip",
    # C-order: np.arange(12).reshape(4,3) = [[0,1,2],[3,4,5],[6,7,8],[9,10,11]]
    # In R (column-major), same logical array is stored transposed:
    data       = matrix(c(0L,3L,6L,9L, 1L,4L,7L,10L, 2L,5L,8L,11L), nrow = 4L, ncol = 3L),
    shape      = c(4L, 3L),
    chunks     = c(2L, 3L),
    dtype_r    = "<i2",
    codec      = "gzip",
    fill_value = 0L
  ),
  list(
    name       = "float64_3d_zstd",
    # C-order: np.arange(24).reshape(2,3,4)
    # Build the R equivalent via aperm of C-order fill
    data       = aperm(array(as.double(0:23), dim = c(4L, 3L, 2L)), c(3, 2, 1)),
    shape      = c(2L, 3L, 4L),
    chunks     = c(2L, 3L, 4L),
    dtype_r    = "<f8",
    codec      = "zstd",
    fill_value = 0
  ),
  list(
    name       = "fill_custom",
    data       = c(1.0, 2.0, 3.0, -9999.0, -9999.0, -9999.0),
    shape      = 6L,
    chunks     = 3L,
    dtype_r    = "<f8",
    codec      = "none",
    fill_value = -9999.0,
    # write all values (including fill region) so both sides are identical
    partial    = FALSE
  ),
  list(
    name       = "ragged_2d_gzip",
    # C-order: np.arange(35).reshape(5,7) — rows filled left-to-right
    data       = matrix(as.integer(0:34), nrow = 5L, ncol = 7L, byrow = TRUE),
    shape      = c(5L, 7L),
    chunks     = c(3L, 4L),
    dtype_r    = "<i4",
    codec      = "gzip",
    fill_value = 0L
  )
)

# ---------------------------------------------------------------------------
# 3. Helpers
# ---------------------------------------------------------------------------
make_compressor <- function(codec) {
  switch(codec,
    gzip = GzipCodec$new(),
    zstd = ZstdCodec$new(),
    none = NA,
    stop("Unknown codec: ", codec)
  )
}

compare_arrays <- function(actual, expected, dtype_r) {
  # Flatten to vectors for comparison
  a <- as.vector(actual)
  e <- as.vector(expected)
  if (length(a) != length(e)) return(FALSE)
  if (grepl("^[<>|]?f", dtype_r)) {
    isTRUE(all.equal(a, e, tolerance = 1e-5))
  } else if (grepl("b1", dtype_r)) {
    identical(as.logical(a), as.logical(e))
  } else {
    identical(as.integer(a), as.integer(e))
  }
}

# Results collector
results <- data.frame(
  direction = character(),
  format    = character(),
  array     = character(),
  status    = character(),
  stringsAsFactors = FALSE
)

add_result <- function(direction, fmt, arr_name, pass) {
  results <<- rbind(results, data.frame(
    direction = direction,
    format    = fmt,
    array     = arr_name,
    status    = if (pass) "PASS" else "FAIL",
    stringsAsFactors = FALSE
  ))
  if (!pass) cat(sprintf("  ** FAIL: %s / %s / %s\n", direction, fmt, arr_name))
}

# ---------------------------------------------------------------------------
# 4. R write phase
# ---------------------------------------------------------------------------
cat("\n--- R write phase ---\n")
for (fmt in c(2L, 3L)) {
  store_path <- file.path(work_dir, paste0("r_v", fmt, ".zarr"))
  dir.create(store_path)
  store <- DirectoryStore$new(store_path)
  root <- zarr_create_group(store = store, zarr_format = fmt)

  for (tc in test_cases) {
    compressor <- make_compressor(tc$codec)
    arr <- root$create_dataset(
      name       = tc$name,
      shape      = as.integer(tc$shape),
      chunks     = as.integer(tc$chunks),
      dtype      = tc$dtype_r,
      fill_value = tc$fill_value,
      compressor = compressor
    )
    arr$set_item("...", tc$data)
  }

  # Group with attributes
  sub <- root$create_group("meta_group")
  sub$get_attrs()$set_item("description", "test group")
  sub$get_attrs()$set_item("version", 1L)

  temp_arr <- sub$create_dataset(
    name       = "temperature",
    shape      = c(3L, 4L),
    chunks     = c(3L, 4L),
    dtype      = "<f4",
    fill_value = 0,
    compressor = GzipCodec$new()
  )
  temp_arr$set_item("...", matrix(c(
    280.1, 281.2, 282.3, 283.4,
    284.5, 285.6, 286.7, 287.8,
    288.9, 290.0, 291.1, 292.2
  ), nrow = 3L, ncol = 4L, byrow = TRUE))
  temp_arr$get_attrs()$set_item("units", "K")

  cat(sprintf("  V%d: wrote %d arrays + meta_group to %s\n",
  fmt, length(test_cases), store_path))
}

# ---------------------------------------------------------------------------
# 5. Run Python write script
# ---------------------------------------------------------------------------
cat("\n--- Python write phase ---\n")

py_work <- normalizePath(work_dir, winslash = "/")
py_out <- system2(python_cmd, c(py_write_script, shQuote(py_work)),
                  stdout = TRUE, stderr = TRUE)
cat(paste(py_out, collapse = "\n"), "\n")
if (!is.null(attr(py_out, "status")) && attr(py_out, "status") != 0) {
  cat("ERROR: Python write script failed:\n")
  cat(paste(py_out, collapse = "\n"), "\n")
  quit(status = 1)
}

# ---------------------------------------------------------------------------
# 6. R reads Python's stores
# ---------------------------------------------------------------------------
cat("\n--- R reads Python stores ---\n")
for (fmt in c(2L, 3L)) {
  py_store_path <- file.path(work_dir, paste0("python_v", fmt, ".zarr"))
  root <- zarr_open_group(py_store_path, mode = "r")

  for (tc in test_cases) {
    actual <- tryCatch(
      root$get_item(tc$name)$as.array(),
      error = function(e) { cat(sprintf("  ERROR reading %s: %s\n", tc$name, e$message)); NULL }
    )
    pass <- if (is.null(actual)) FALSE else compare_arrays(actual, tc$data, tc$dtype_r)
    add_result("R reads Py", paste0("V", fmt), tc$name, pass)
  }

  # Check meta_group attributes
  sub <- root$get_item("meta_group")
  attrs <- sub$get_attrs()$to_list()
  attr_pass <- identical(attrs$description, "test group") && identical(as.integer(attrs$version), 1L)
  add_result("R reads Py", paste0("V", fmt), "meta_group/attrs", attr_pass)

  # Check temperature array + attributes
  temp <- sub$get_item("temperature")
  temp_data <- temp$as.array()
  temp_expected <- matrix(c(
    280.1, 281.2, 282.3, 283.4,
    284.5, 285.6, 286.7, 287.8,
    288.9, 290.0, 291.1, 292.2
  ), nrow = 3L, ncol = 4L, byrow = TRUE)
  temp_pass <- compare_arrays(temp_data, temp_expected, "<f4")
  add_result("R reads Py", paste0("V", fmt), "meta_group/temperature", temp_pass)

  temp_attrs <- temp$get_attrs()$to_list()
  add_result("R reads Py", paste0("V", fmt), "meta_group/temperature/attrs",
  identical(temp_attrs$units, "K"))
}

# ---------------------------------------------------------------------------
# 7. Run Python read script
# ---------------------------------------------------------------------------
cat("\n--- Python reads R stores ---\n")

arr_names <- paste(vapply(test_cases, function(tc) tc$name, ""), collapse = ",")
py_json <- system2(python_cmd, c(py_read_script, shQuote(py_work), shQuote(arr_names)),
                   stdout = TRUE, stderr = TRUE)
if (!is.null(attr(py_json, "status")) && attr(py_json, "status") != 0) {
  cat("ERROR: Python read script failed:\n")
  cat(paste(py_json, collapse = "\n"), "\n")
  quit(status = 1)
}

py_results <- jsonlite::fromJSON(paste(py_json, collapse = ""))

for (i in seq_len(nrow(py_results))) {
  row <- py_results[i, ]
  fmt_str <- paste0("V", row$format)

  if (!row$ok) {
    add_result("Py reads R", fmt_str, row$name, FALSE)
    next
  }

  # Attribute checks
  if (row$name == "meta_group/attrs") {
    d <- row$data[[1]]
    pass <- identical(d$description, "test group") && identical(as.integer(d$version), 1L)
    add_result("Py reads R", fmt_str, row$name, pass)
    next
  }
  if (row$name == "meta_group/temperature/attrs") {
    d <- row$data[[1]]
    pass <- identical(d$units, "K")
    add_result("Py reads R", fmt_str, row$name, pass)
    next
  }

  # Array data checks
  tc_match <- Filter(function(tc) tc$name == row$name, test_cases)
  if (length(tc_match) == 0 && row$name == "meta_group/temperature") {
    expected_flat <- c(280.1, 281.2, 282.3, 283.4, 284.5, 285.6,
      286.7, 287.8, 288.9, 290.0, 291.1, 292.2)
      actual_flat <- unlist(row$data)
      pass <- isTRUE(all.equal(actual_flat, expected_flat, tolerance = 1e-4))
      add_result("Py reads R", fmt_str, row$name, pass)
      next
    }

    tc <- tc_match[[1]]
    actual_flat <- unlist(row$data)
    # Python flattens C-order; for multi-dim R arrays, we need C-order flat too
    if (is.array(tc$data) || is.matrix(tc$data)) {
      expected_flat <- as.vector(aperm(tc$data))
    } else {
      expected_flat <- as.vector(tc$data)
    }

    if (grepl("^[<>|]?f", tc$dtype_r)) {
      pass <- isTRUE(all.equal(as.double(actual_flat), as.double(expected_flat),
      tolerance = 1e-5))
    } else if (grepl("b1", tc$dtype_r)) {
      pass <- identical(as.logical(actual_flat), as.logical(expected_flat))
    } else {
      pass <- identical(as.integer(actual_flat), as.integer(expected_flat))
    }
    add_result("Py reads R", fmt_str, row$name, pass)
  }

  # ---------------------------------------------------------------------------
  # 8. Summary
  # ---------------------------------------------------------------------------
  cat("\n=== pizzarr Cross-Validation Report ===\n")
  cat(sprintf("Python: zarr %s | R: pizzarr %s\n\n",
  zarr_ver, packageVersion("pizzarr")))

  cat(sprintf("%-14s %-6s %-32s %s\n", "Direction", "Format", "Array", "Status"))
  cat(paste(rep("-", 66), collapse = ""), "\n")
  for (i in seq_len(nrow(results))) {
    r <- results[i, ]
    cat(sprintf("%-14s %-6s %-32s %s\n", r$direction, r$format, r$array, r$status))
  }

  n_pass <- sum(results$status == "PASS")
  n_fail <- sum(results$status == "FAIL")
  cat(sprintf("\nResults: %d/%d PASS, %d FAIL\n", n_pass, nrow(results), n_fail))

  if (n_fail > 0) {
    cat("\nFailed checks:\n")
    fails <- results[results$status == "FAIL", ]
    for (i in seq_len(nrow(fails))) {
      cat(sprintf("  - %s / %s / %s\n", fails$direction[i], fails$format[i], fails$array[i]))
    }
  }

  quit(status = as.integer(n_fail > 0))
