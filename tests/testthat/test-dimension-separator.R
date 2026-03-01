library(pizzarr)

# --- Dimension separator behavior tests ---

test_that("Array with '/' dimension separator writes and reads correctly (MemoryStore)", {
  store <- MemoryStore$new()
  a <- zarr_create(
    shape = c(4, 4), chunks = c(2, 2),
    store = store, dimension_separator = "/"
  )
  data <- array(1:16, dim = c(4, 4))
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

test_that("Array with '.' dimension separator writes and reads correctly (MemoryStore)", {
  store <- MemoryStore$new()
  a <- zarr_create(
    shape = c(4, 4), chunks = c(2, 2),
    store = store, dimension_separator = "."
  )
  data <- array(1:16, dim = c(4, 4))
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

test_that("Default dimension separator is '.'", {
  store <- DirectoryStore$new(tempfile())
  a <- zarr_create(shape = c(4, 4), chunks = c(2, 2), store = store)
  data <- array(1:16, dim = c(4, 4))
  a$set_item("...", data)
  # Chunks should use "." separator: "0.0", "0.1", "1.0", "1.1"
  expect_true(store$contains_item("0.0"))
  expect_true(store$contains_item("0.1"))
  expect_true(store$contains_item("1.0"))
  expect_true(store$contains_item("1.1"))
})

test_that("Chunk keys use '/' separator when specified (DirectoryStore)", {
  store <- DirectoryStore$new(tempfile())
  a <- zarr_create(
    shape = c(4, 4), chunks = c(2, 2),
    store = store, dimension_separator = "/"
  )
  data <- array(1:16, dim = c(4, 4))
  a$set_item("...", data)
  # Chunks should use "/" separator: "0/0", "0/1", "1/0", "1/1"
  expect_true(store$contains_item("0/0"))
  expect_true(store$contains_item("0/1"))
  expect_true(store$contains_item("1/0"))
  expect_true(store$contains_item("1/1"))
})

test_that("1D array chunk keys are plain integers", {
  store <- DirectoryStore$new(tempfile())
  a <- zarr_create(shape = c(6), chunks = c(3), store = store)
  data <- array(1:6, dim = c(6))
  a$set_item("...", data)
  expect_true(store$contains_item("0"))
  expect_true(store$contains_item("1"))
})

test_that("Dimension separator is preserved in metadata round-trip (DirectoryStore)", {
  d <- tempfile()
  store <- DirectoryStore$new(d)
  a <- zarr_create(
    shape = c(4, 4), chunks = c(2, 2),
    store = store, dimension_separator = "/"
  )
  data <- array(1:16, dim = c(4, 4))
  a$set_item("...", data)
  # Reopen from same directory
  b <- zarr_open_array(d, mode = "r")
  result <- b$get_item("...")
  expect_equal(result$data, data)
})
