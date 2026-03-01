library(pizzarr)

test_that("Zarr Array can load .zarray metadata", {
  store <- MemoryStore$new()

  zarray_meta <- store$metadata_class$encode_array_metadata(create_zarray_meta(
    dtype = Dtype$new("|u1"),
    order = "C",
    fill_value = 0,
    shape = c(1, 2),
    chunks = c(3, 4),
    dimension_separator = "."
  ))

  store$set_item(".zarray", zarray_meta)
  
  array <- ZarrArray$new(store = store)
  shape <- array$get_shape()
  
  expect_equal(shape, c(1, 2))
})

test_that("Zarr Array can be resized", {
  store <- MemoryStore$new()
  
  zarray_meta <- store$metadata_class$encode_array_metadata(create_zarray_meta(
    dtype = Dtype$new("|u1"),
    order = "C",
    fill_value = 0,
    shape = c(4, 5),
    chunks = c(3, 4),
    dimension_separator = "."
  ))
  
  store$set_item(".zarray", zarray_meta)
  
  array <- ZarrArray$new(store = store)
  old_shape <- array$get_shape()
  expect_equal(old_shape, c(4, 5))
  array$resize(1, 2)
  
  new_shape <- array$get_shape()
  expect_equal(new_shape, c(1, 2))
})

# --- Write/read integration tests ---

test_that("Array write/read round-trip, MemoryStore, no compressor", {
  a <- zarr_create(
    shape = c(4, 4), chunks = c(2, 2),
    store = MemoryStore$new(), compressor = NA
  )
  data <- array(1:16, dim = c(4, 4))
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

test_that("Array write/read round-trip, MemoryStore, Zstd compressor", {
  a <- zarr_create(
    shape = c(4, 4), chunks = c(2, 2),
    store = MemoryStore$new(), compressor = ZstdCodec$new()
  )
  data <- array(1:16, dim = c(4, 4))
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

test_that("Array write/read round-trip, DirectoryStore, default compressor", {
  a <- zarr_create(
    shape = c(4, 4), chunks = c(2, 2),
    store = DirectoryStore$new(tempfile())
  )
  data <- array(1:16, dim = c(4, 4))
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

test_that("Array persists to DirectoryStore and reopens correctly", {
  d <- tempfile()
  data <- array(1:20, dim = c(4, 5))
  a <- zarr_create(shape = c(4, 5), chunks = c(2, 5), store = d)
  a$set_item("...", data)
  b <- zarr_open_array(d, mode = "r")
  result <- b$get_item("...")
  expect_equal(result$data, data)
})

test_that("1D array write/read round-trip", {
  a <- zarr_create(
    shape = c(10), chunks = c(5),
    store = MemoryStore$new(), compressor = NA
  )
  data <- array(1:10, dim = c(10))
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

test_that("3D array write/read round-trip", {
  a <- zarr_create(
    shape = c(3, 3, 3), chunks = c(2, 2, 2),
    store = MemoryStore$new(), compressor = NA
  )
  data <- array(1:27, dim = c(3, 3, 3))
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

test_that("Array with fill_value=0 returns fill for unwritten chunks", {
  a <- zarr_create(
    shape = c(4), chunks = c(2),
    store = MemoryStore$new(), fill_value = 0, compressor = NA
  )
  result <- a$get_item("...")
  expect_equal(as.vector(result$data), c(0, 0, 0, 0))
})

test_that("Array metadata properties are correct", {
  a <- zarr_create(
    shape = c(10, 20), chunks = c(5, 10), dtype = "<f4",
    store = MemoryStore$new(), compressor = ZstdCodec$new()
  )
  expect_equal(a$get_shape(), c(10, 20))
  expect_equal(a$get_chunks(), c(5, 10))
  expect_s3_class(a$get_compressor(), "ZstdCodec")
})
