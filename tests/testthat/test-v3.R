library(pizzarr)

# Unzip V3 test fixture to temp directory.
# Fixture generated with zarrita (see inst/extdata/fixtures/v3/generate-v3.py).
v3_zip <- system.file("extdata/fixtures/v3/data.zarr.zip", package = "pizzarr")
tdir <- tempfile("v3test")
dir.create(tdir)
utils::unzip(v3_zip, exdir = tdir)
v3_root <- file.path(tdir, "data.zarr")

# --- Format detection ---

test_that("detect_zarr_version returns 3 for V3 store", {
  store <- DirectoryStore$new(v3_root)
  expect_equal(detect_zarr_version(store), 3L)
})

test_that("contains_group detects V3 root group", {
  store <- DirectoryStore$new(v3_root)
  expect_true(contains_group(store))
})

test_that("contains_array detects V3 array", {
  store <- DirectoryStore$new(v3_root)
  expect_true(contains_array(store, "1d.contiguous.raw.i2"))
})

# --- Group opening ---

test_that("V3 group can be opened with zarr_open_group", {
  g <- zarr_open_group(v3_root, mode = "r")
  expect_s3_class(g, "ZarrGroup")
})

test_that("V3 group can be opened with zarr_open", {
  g <- zarr_open(v3_root, mode = "r")
  expect_s3_class(g, "ZarrGroup")
})

# --- 1D arrays ---

test_that("V3 1d contiguous raw (no compression) int16", {
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "1d.contiguous.raw.i2", read_only = TRUE)
  expect_equal(a$get_shape(), 4)
  result <- a$get_item("...")
  expect_equal(as.integer(result$data), c(1L, 2L, 3L, 4L))
})

test_that("V3 1d contiguous gzip int16", {
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "1d.contiguous.gzip.i2", read_only = TRUE)
  expect_equal(a$get_shape(), 4)
  result <- a$get_item("...")
  expect_equal(as.integer(result$data), c(1L, 2L, 3L, 4L))
})

test_that("V3 1d contiguous blosc int16", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "1d.contiguous.blosc.i2", read_only = TRUE)
  expect_equal(a$get_shape(), 4)
  result <- a$get_item("...")
  expect_equal(as.integer(result$data), c(1L, 2L, 3L, 4L))
})

test_that("V3 1d contiguous int32", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "1d.contiguous.i4", read_only = TRUE)
  expect_equal(a$get_shape(), 4)
  result <- a$get_item("...")
  expect_equal(as.integer(result$data), c(1L, 2L, 3L, 4L))
})

test_that("V3 1d contiguous uint8", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "1d.contiguous.u1", read_only = TRUE)
  expect_equal(a$get_shape(), 4)
  result <- a$get_item("...")
  expect_equal(as.integer(result$data), c(255L, 0L, 255L, 0L))
})

test_that("V3 1d contiguous float32 little-endian", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "1d.contiguous.f4.le", read_only = TRUE)
  expect_equal(a$get_shape(), 4)
  result <- a$get_item("...")
  expect_equal(as.double(result$data), c(-1000.5, 0, 1000.5, 0))
})

test_that("V3 1d contiguous float32 big-endian", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "1d.contiguous.f4.be", read_only = TRUE)
  expect_equal(a$get_shape(), 4)
  result <- a$get_item("...")
  expect_equal(as.double(result$data), c(-1000.5, 0, 1000.5, 0))
})

test_that("V3 1d contiguous float64", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "1d.contiguous.f8", read_only = TRUE)
  expect_equal(a$get_shape(), 4)
  result <- a$get_item("...")
  expect_equal(as.double(result$data), c(1.5, 2.5, 3.5, 4.5))
})

test_that("V3 1d contiguous bool", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "1d.contiguous.b1", read_only = TRUE)
  expect_equal(a$get_shape(), 4)
  result <- a$get_item("...")
  expect_equal(as.logical(result$data), c(TRUE, FALSE, TRUE, FALSE))
})

test_that("V3 1d chunked int16", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "1d.chunked.i2", read_only = TRUE)
  expect_equal(a$get_shape(), 4)
  result <- a$get_item("...")
  expect_equal(as.integer(result$data), c(1L, 2L, 3L, 4L))
})

test_that("V3 1d chunked ragged int16", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "1d.chunked.ragged.i2", read_only = TRUE)
  expect_equal(a$get_shape(), 5)
  result <- a$get_item("...")
  expect_equal(as.integer(result$data), c(1L, 2L, 3L, 4L, 5L))
})

# --- 2D arrays ---

test_that("V3 2d contiguous int16", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "2d.contiguous.i2", read_only = TRUE)
  expect_equal(a$get_shape(), c(2L, 2L))
  result <- a$get_item("...")
  # Data written as [[1,2],[3,4]] in C order (row-major).
  # R uses column-major, so check element access directly.
  expect_equal(result$data[1, 1], 1)
  expect_equal(result$data[1, 2], 2)
  expect_equal(result$data[2, 1], 3)
  expect_equal(result$data[2, 2], 4)
})

test_that("V3 2d chunked int16", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "2d.chunked.i2", read_only = TRUE)
  expect_equal(a$get_shape(), c(2L, 2L))
  result <- a$get_item("...")
  expect_equal(result$data[1, 1], 1)
  expect_equal(result$data[1, 2], 2)
  expect_equal(result$data[2, 1], 3)
  expect_equal(result$data[2, 2], 4)
})

test_that("V3 2d chunked ragged int16", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "2d.chunked.ragged.i2", read_only = TRUE)
  expect_equal(a$get_shape(), c(3L, 3L))
  result <- a$get_item("...")
  # Data written as [[1,2,3],[4,5,6],[7,8,9]] in C order.
  expect_equal(result$data[1, 1], 1)
  expect_equal(result$data[1, 3], 3)
  expect_equal(result$data[2, 1], 4)
  expect_equal(result$data[3, 3], 9)
})

# --- 3D arrays ---

test_that("V3 3d contiguous int16", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "3d.contiguous.i2", read_only = TRUE)
  expect_equal(a$get_shape(), c(3L, 3L, 3L))
  result <- a$get_item("...")
  # Data written as np.arange(27).reshape(3,3,3) in C order.
  # In C order: element[i,j,k] = i*9 + j*3 + k (0-based).
  # Check element access (R 1-based): data[i,j,k] = (i-1)*9 + (j-1)*3 + (k-1)
  expect_equal(result$data[1, 1, 1], 0)
  expect_equal(result$data[1, 1, 2], 1)
  expect_equal(result$data[1, 2, 1], 3)
  expect_equal(result$data[2, 1, 1], 9)
  expect_equal(result$data[3, 3, 3], 26)
  expect_equal(length(result$data), 27)
})

test_that("V3 3d chunked int16", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "3d.chunked.i2", read_only = TRUE)
  expect_equal(a$get_shape(), c(3L, 3L, 3L))
  result <- a$get_item("...")
  expect_equal(result$data[1, 1, 1], 0)
  expect_equal(result$data[2, 1, 1], 9)
  expect_equal(result$data[3, 3, 3], 26)
  expect_equal(length(result$data), 27)
})

test_that("V3 3d chunked mixed C-order int16", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "3d.chunked.mixed.i2.C", read_only = TRUE)
  expect_equal(a$get_shape(), c(3L, 3L, 3L))
  result <- a$get_item("...")
  expect_equal(result$data[1, 1, 1], 0)
  expect_equal(result$data[2, 1, 1], 9)
  expect_equal(result$data[3, 3, 3], 26)
  expect_equal(length(result$data), 27)
})

test_that("V3 3d chunked mixed F-order int16", {
  skip_if_not_installed("Rarr")
  store <- DirectoryStore$new(v3_root)
  a <- ZarrArray$new(store, path = "3d.chunked.mixed.i2.F", read_only = TRUE)
  expect_equal(a$get_shape(), c(3L, 3L, 3L))
  result <- a$get_item("...")
  expect_equal(result$data[1, 1, 1], 0)
  expect_equal(result$data[2, 1, 1], 9)
  expect_equal(result$data[3, 3, 3], 26)
  expect_equal(length(result$data), 27)
})

# --- Auto-detection via zarr_open ---

test_that("zarr_open auto-detects V3 group and can read child array", {
  g <- zarr_open(v3_root, mode = "r")
  expect_s3_class(g, "ZarrGroup")
  arr <- g$get_item("1d.contiguous.raw.i2")
  expect_s3_class(arr, "ZarrArray")
  result <- arr$get_item("...")
  expect_equal(as.integer(result$data), c(1L, 2L, 3L, 4L))
})

# --- V3 write guard ---

test_that("V3 store forces read-only when non-read mode requested", {
  expect_message(
    g <- zarr_open_group(v3_root, mode = "a"),
    "V3 write support not yet available"
  )
  expect_true(g$get_read_only())
})

# Clean up
unlink(tdir, recursive = TRUE)

# =============================================================================
# zarr-python 3.x interop tests
# Fixture generated with zarr-python 3.1.5
# (see inst/extdata/fixtures/v3/generate-v3-zarr-python.py)
# =============================================================================

zp_zip <- system.file("extdata/fixtures/v3/zarr_python_v3.zarr.zip",
                       package = "pizzarr")
zp_dir <- tempfile("zp_v3test")
dir.create(zp_dir)
utils::unzip(zp_zip, exdir = zp_dir)
zp_root <- file.path(zp_dir, "zarr_python_v3.zarr")

test_that("zarr-python V3 store opens as group", {
  g <- zarr_open(zp_root, mode = "r")
  expect_s3_class(g, "ZarrGroup")
  attrs <- g$get_attrs()$to_list()
  expect_equal(attrs$generator, "zarr-python")
  expect_equal(attrs$zarr_python_version, "3.1.5")
})

test_that("zarr-python V3 int32 1d (gzip)", {
  g <- zarr_open(zp_root, mode = "r")
  a <- g$get_item("int32_1d")
  expect_equal(a$get_shape(), 6)
  expect_equal(as.integer(a$as.array()), c(10L, 20L, 30L, 40L, 50L, 60L))
})

test_that("zarr-python V3 float64 1d (zstd)", {
  g <- zarr_open(zp_root, mode = "r")
  a <- g$get_item("float64_1d")
  expect_equal(a$get_shape(), 5)
  expect_equal(as.double(a$as.array()), c(1.1, 2.2, 3.3, 4.4, 5.5))
})

test_that("zarr-python V3 bool 1d (uncompressed)", {
  g <- zarr_open(zp_root, mode = "r")
  a <- g$get_item("bool_1d")
  expect_equal(a$get_shape(), 4)
  expect_equal(as.logical(a$as.array()), c(TRUE, FALSE, TRUE, FALSE))
})

test_that("zarr-python V3 uint8 1d (uncompressed, chunked)", {
  g <- zarr_open(zp_root, mode = "r")
  a <- g$get_item("uint8_1d")
  expect_equal(a$get_shape(), 4)
  expect_equal(as.integer(a$as.array()), c(0L, 127L, 128L, 255L))
})

test_that("zarr-python V3 float32 1d (blosc)", {
  skip_if_not_installed("Rarr")
  g <- zarr_open(zp_root, mode = "r")
  a <- g$get_item("float32_1d")
  expect_equal(a$get_shape(), 4)
  result <- as.double(a$as.array())
  expect_equal(result[1], -1.5)
  expect_equal(result[2], 0.0)
  expect_equal(result[3], 1.5)
  expect_equal(result[4], 3.14, tolerance = 1e-5)
})

test_that("zarr-python V3 int16 2d (gzip)", {
  g <- zarr_open(zp_root, mode = "r")
  a <- g$get_item("int16_2d")
  expect_equal(a$get_shape(), c(4, 3))
  m <- a$as.array()
  expect_equal(m[1, 1], 0)
  expect_equal(m[1, 3], 2)
  expect_equal(m[4, 3], 11)
})

test_that("zarr-python V3 float64 3d (zstd)", {
  g <- zarr_open(zp_root, mode = "r")
  a <- g$get_item("float64_3d")
  expect_equal(a$get_shape(), c(2, 3, 4))
  arr <- a$as.array()
  expect_equal(arr[1, 1, 1], 0)
  expect_equal(arr[2, 3, 4], 23)
})

test_that("zarr-python V3 fill value works", {
  g <- zarr_open(zp_root, mode = "r")
  a <- g$get_item("with_fill")
  expect_equal(a$get_shape(), 6)
  d <- as.double(a$as.array())
  expect_equal(d[1:3], c(1.0, 2.0, 3.0))
  expect_equal(d[4:6], c(-9999.0, -9999.0, -9999.0))
})

test_that("zarr-python V3 ragged 2d (gzip)", {
  g <- zarr_open(zp_root, mode = "r")
  a <- g$get_item("ragged_2d")
  expect_equal(a$get_shape(), c(5, 7))
  m <- a$as.array()
  expect_equal(m[1, 1], 0)
  expect_equal(m[5, 7], 34)
  expect_equal(length(m), 35)
})

test_that("zarr-python V3 nested group with attributes", {
  g <- zarr_open(zp_root, mode = "r")
  sub <- g$get_item("var_group")
  expect_s3_class(sub, "ZarrGroup")
  attrs <- sub$get_attrs()$to_list()
  expect_equal(attrs$description, "Group with multiple variables")
  expect_equal(attrs$source, "zarr-python 3.x test generation")
})

test_that("zarr-python V3 sub-group array with attributes (temperature)", {
  g <- zarr_open(zp_root, mode = "r")
  temp <- g$get_item("var_group")$get_item("temperature")
  expect_equal(temp$get_shape(), c(3, 4))
  ta <- temp$get_attrs()$to_list()
  expect_equal(ta$units, "K")
  expect_equal(ta$long_name, "Temperature")
  m <- temp$as.array()
  expect_equal(m[1, 1], 280.1, tolerance = 1e-1)
  expect_equal(m[3, 4], 292.2, tolerance = 1e-1)
})

test_that("zarr-python V3 sub-group array (pressure, gzip)", {
  g <- zarr_open(zp_root, mode = "r")
  pres <- g$get_item("var_group")$get_item("pressure")
  expect_equal(pres$get_shape(), c(3, 4))
  m <- pres$as.array()
  expect_equal(m[1, 1], 101325.0)
  expect_equal(m[3, 4], 101270.0)
})

# Clean up
unlink(zp_dir, recursive = TRUE)
