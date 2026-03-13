library(pizzarr)

test_that("1D append works", {
  z <- zarr_create(3, chunks = 3, dtype = "<f4")
  z$set_item("...", array(c(1, 2, 3)))

  z$append(array(c(4, 5)))
  expect_equal(z$get_shape(), c(5))
  expect_equal(as.numeric(z$get_item("...")$data), c(1, 2, 3, 4, 5))
})

test_that("2D append along axis 1 (rows)", {
  z <- zarr_create(c(2, 3), chunks = c(2, 3), dtype = "<f4")
  original <- array(1:6, dim = c(2, 3))
  z$set_item("...", original)

  new_row <- array(7:9, dim = c(1, 3))
  z$append(new_row, axis = 1)
  expect_equal(z$get_shape(), c(3, 3))

  result <- z$get_item("...")$data
  expect_equal(as.numeric(result[1:2, ]), as.numeric(original))
  expect_equal(as.numeric(result[3, ]), c(7, 8, 9))
})

test_that("append errors on dimension mismatch", {
  z <- zarr_create(c(2, 3), chunks = c(2, 3), dtype = "<f4")
  z$set_item("...", array(1:6, dim = c(2, 3)))

  expect_error(z$append(array(1:3)), "incompatible number of dimensions")
})

test_that("append errors on shape mismatch on non-append axis", {
  z <- zarr_create(c(2, 3), chunks = c(2, 3), dtype = "<f4")
  z$set_item("...", array(1:6, dim = c(2, 3)))

  bad_data <- array(1:4, dim = c(1, 4))
  expect_error(z$append(bad_data, axis = 1), "shape mismatch")
})

test_that("append converts non-array to array automatically", {
  a <- zarr_create(
    shape = c(4), chunks = c(2), store = MemoryStore$new(),
    compressor = NA, fill_value = 0
  )
  a$set_item("...", array(1:4, dim = c(4)))
  # Pass a plain vector, not an array — triggers as.array() path
  a$append(5:8, axis = 1)
  expect_equal(a$get_shape(), c(8))
  result <- a$get_item("...")
  expect_equal(as.vector(result$data), 1:8)
})

test_that("append raises error for wrong number of dimensions", {
  a <- zarr_create(
    shape = c(4, 2), chunks = c(2, 2), store = MemoryStore$new(), compressor = NA
  )
  expect_error(a$append(array(1:4, dim = c(4)), axis = 1), "dimensions")
})

test_that("append raises error for non-append axis shape mismatch", {
  a <- zarr_create(
    shape = c(4, 2), chunks = c(2, 2), store = MemoryStore$new(), compressor = NA
  )
  expect_error(a$append(array(1:9, dim = c(3, 3)), axis = 1), "shape mismatch")
})
