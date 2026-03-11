test_that("is_int64_dtype identifies 64-bit integer dtypes", {
  d_i8 <- Dtype$new("<i8")
  d_u8 <- suppressWarnings(Dtype$new("<u8"))
  d_i4 <- Dtype$new("<i4")
  d_f8 <- Dtype$new("<f8")

  expect_true(is_int64_dtype(d_i8))
  expect_true(is_int64_dtype(d_u8))
  expect_false(is_int64_dtype(d_i4))
  expect_false(is_int64_dtype(d_f8))
})

test_that("uint64 dtype warns about signed overflow", {
  expect_warning(Dtype$new("<u8"), "uint64")
})

test_that("get_dtype_rtype returns integer64 for 8-byte ints when bit64 available", {
  skip_if_not_installed("bit64")
  rtype <- get_dtype_rtype("i", num_bytes = 8)
  expect_s3_class(rtype, "integer64")
})

test_that("get_dtype_rtype returns integer for 4-byte ints", {
  rtype <- get_dtype_rtype("i", num_bytes = 4)
  expect_true(is.integer(rtype))
})

test_that("get_dtype_asrtype returns as.integer64 for int64 dtype", {
  skip_if_not_installed("bit64")
  func <- get_dtype_asrtype("<i8")
  expect_identical(func, bit64::as.integer64)
})

test_that("raw_to_integer64 round-trips correctly", {
  skip_if_not_installed("bit64")

  vals <- bit64::as.integer64(c(0, 1, -1, 2147483648, -2147483649))
  raw_bytes <- integer64_to_raw(vals, endian = "little")
  result <- raw_to_integer64(raw_bytes, n = 5, endian = "little")
  expect_true(inherits(result, "integer64"))
  expect_true(all(result == vals))
})

test_that("raw_to_integer64 handles big endian", {
  skip_if_not_installed("bit64")

  vals <- bit64::as.integer64(c(42, 123456789012345))
  raw_bytes <- integer64_to_raw(vals, endian = "big")
  result <- raw_to_integer64(raw_bytes, n = 2, endian = "big")
  expect_true(all(result == vals))
})

test_that("NestedArray round-trips int64 data from raw", {
  skip_if_not_installed("bit64")

  vals <- bit64::as.integer64(c(1, 2, 3000000000, 4000000000))
  dtype <- Dtype$new("<i8")
  raw_bytes <- integer64_to_raw(vals, endian = "little")

  na <- NestedArray$new(data = raw_bytes, shape = c(4), dtype = dtype)
  expect_s3_class(na$data, "integer64")
  data_vec <- na$data
  dim(data_vec) <- NULL
  expect_true(all(data_vec == vals))

  # Round-trip back to raw
  raw_out <- na$flatten_to_raw()
  expect_equal(raw_out, raw_bytes)
})

test_that("NestedArray round-trips int64 2D array", {
  skip_if_not_installed("bit64")

  vals <- bit64::as.integer64(c(10, 20, 30, 40, 50, 60))
  dtype <- Dtype$new("<i8")
  raw_bytes <- integer64_to_raw(vals, endian = "little")

  na <- NestedArray$new(data = raw_bytes, shape = c(2, 3), dtype = dtype)
  expect_s3_class(na$data, "integer64")
  expect_equal(dim(na$data), c(2, 3))

  # Round-trip
  raw_out <- na$flatten_to_raw()
  expect_equal(raw_out, raw_bytes)
})

test_that("int64 NestedArray handles C order", {
  skip_if_not_installed("bit64")

  vals <- bit64::as.integer64(c(1, 2, 3, 4, 5, 6))
  dtype <- Dtype$new("<i8")
  raw_bytes <- integer64_to_raw(vals, endian = "little")

  na <- NestedArray$new(data = raw_bytes, shape = c(2, 3), dtype = dtype,
                        order = "C")
  expect_s3_class(na$data, "integer64")

  # Round-trip with C order
  raw_out <- na$flatten_to_raw(order = "C")
  expect_equal(raw_out, raw_bytes)
})

test_that("normalize_fill_value works for int64 dtype", {
  skip_if_not_installed("bit64")

  dtype <- Dtype$new("<i8")
  fv <- normalize_fill_value(0, dtype)
  expect_s3_class(fv, "integer64")
  expect_true(fv == bit64::as.integer64(0))

  fv2 <- normalize_fill_value(42, dtype)
  expect_s3_class(fv2, "integer64")
  expect_true(fv2 == bit64::as.integer64(42))
})

test_that("int64 zarr array round-trip via DirectoryStore", {
  skip_if_not_installed("bit64")

  dir <- file.path(tempdir(TRUE), "test_int64.zarr")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  store <- DirectoryStore$new(dir)
  za <- zarr_create(shape = c(4), chunks = c(4), dtype = "<i8",
                    store = store, overwrite = TRUE)

  vals <- bit64::as.integer64(c(1, 2, 3000000000, -3000000000))
  za$set_item("...", vals)

  result <- za$get_item("...")$data
  expect_s3_class(result, "integer64")
  result_vec <- result
  dim(result_vec) <- NULL
  expect_true(all(result_vec == vals))
})

test_that("int64 zarr array round-trip via MemoryStore", {
  skip_if_not_installed("bit64")

  store <- MemoryStore$new()
  za <- zarr_create(shape = c(6), chunks = c(3), dtype = "<i8",
                    store = store, overwrite = TRUE)

  vals <- bit64::as.integer64(c(100, 200, 300, 400, 500, 600))
  za$set_item("...", vals)

  result <- za$get_item("...")$data
  expect_s3_class(result, "integer64")
  result_vec <- result
  dim(result_vec) <- NULL
  expect_true(all(result_vec == vals))
})
