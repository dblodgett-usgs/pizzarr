library(pizzarr)

test_that("zarr_create_empty", {
  z <- zarr_create_empty(100, chunks=10)
  shape <- z$get_shape()
  chunks <- z$get_chunks()
  
  expect_equal(shape, c(100))
  expect_equal(chunks, c(10))
})


test_that("zarr_create_zeros", {
  z <- zarr_create_zeros(100, chunks=10)
  shape <- z$get_shape()
  chunks <- z$get_chunks()
  
  expect_equal(shape, c(100))
  expect_equal(chunks, c(10))
})

test_that("zarr_create zero-dimensional", {
    a <- as_scalar(42)
    z <- zarr_create(shape=dim(a), dtype="<f4", fill_value=NA)
    expect_equal(z$get_shape(), NULL)
})

test_that("zarr_save_array and zarr_open_array", {
    a <- array(data=1:20, dim=c(2, 10))
    z <- zarr_create_array(data=a, shape=dim(a), dtype="<f4", fill_value=NA)
    zarr_save_array(file.path("test_data", "test_zarr_save_array.zarr"), z, overwrite = TRUE)

    z2 <- zarr_open_array(file.path("test_data", "test_zarr_save_array.zarr"))
    selection <- z2$get_item("...")

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z2$get_shape(), c(2, 10))

    expect_equal(a, selection$data)

    unlink(file.path("test_data", "test_zarr_save_array.zarr"), recursive = TRUE)
})

test_that("BadCompressorError on invalid compressor", {
  bad_compressor <- list(not_a_codec = TRUE)
  expect_error(
    zarr_create(shape = c(10), chunks = c(10), dtype = "<f4",
                compressor = bad_compressor),
    "BadCompressorError"
  )
})

test_that("normalize_object_codec errors for object dtype without codec", {
  dtype <- Dtype$new("|O")
  expect_error(
    normalize_object_codec(dtype, NA),
    "missing object_codec for object array"
  )
})

test_that("normalize_object_codec warns for non-object dtype with codec", {
  dtype <- Dtype$new("<f8")
  codec <- VLenUtf8Codec$new()
  expect_warning(
    normalize_object_codec(dtype, codec),
    "an object_codec is only needed for object arrays"
  )
})