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
