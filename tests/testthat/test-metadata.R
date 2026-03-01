library(pizzarr)

# --- Metadata2 encode/decode round-trip ---

test_that("Metadata2 encode/decode round-trips a simple list", {
  m <- Metadata2$new()
  data <- list(foo = "bar", num = 42)
  encoded <- m$encode_metadata(data)
  expect_type(encoded, "raw")
  decoded <- m$decode_metadata(encoded)
  expect_equal(decoded$foo, list("bar"))
  expect_equal(decoded$num, list(42))
})

test_that("Metadata2 decode_metadata handles list input (passthrough)", {
  m <- Metadata2$new()
  data <- list(foo = "bar")
  result <- m$decode_metadata(data)
  expect_equal(result, data)
})

test_that("Metadata2 decode_metadata handles NULL input", {
  m <- Metadata2$new()
  result <- m$decode_metadata(NULL)
  expect_null(result)
})

# --- Array metadata ---

test_that("Metadata2 encode_array_metadata adds zarr_format=2", {
  m <- Metadata2$new()
  meta <- list(
    shape = list(10, 20),
    chunks = list(5, 5),
    dtype = jsonlite::unbox("<f8"),
    compressor = jsonlite::unbox(NA),
    fill_value = jsonlite::unbox(0),
    order = jsonlite::unbox("C"),
    filters = jsonlite::unbox(NA)
  )
  encoded <- m$encode_array_metadata(meta)
  decoded <- m$decode_array_metadata(encoded)
  expect_equal(decoded$zarr_format, 2)
})

# --- Group metadata ---

test_that("Metadata2 encode/decode group metadata round-trips", {
  m <- Metadata2$new()
  encoded <- m$encode_group_metadata()
  decoded <- m$decode_group_metadata(encoded)
  expect_equal(decoded$zarr_format, 2)
})

# --- Validation ---

test_that("validate_v2_meta rejects zarr_format != 2", {
  expect_error(validate_v2_meta(list(zarr_format = 3)), "unsupported zarr format")
  expect_error(validate_v2_meta(list(zarr_format = 1)), "unsupported zarr format")
})

test_that("validate_v2_meta accepts zarr_format = 2", {
  expect_silent(validate_v2_meta(list(zarr_format = 2)))
})

# --- create_zarray_meta ---

test_that("create_zarray_meta with valid inputs returns correct structure", {
  dtype <- Dtype$new("<f8")
  meta <- create_zarray_meta(
    shape = c(10, 20),
    chunks = c(5, 5),
    dtype = dtype,
    compressor = NA,
    fill_value = 0,
    order = "C",
    filters = NA,
    dimension_separator = "."
  )
  expect_equal(meta$shape, c(10, 20))
  expect_equal(meta$chunks, c(5, 5))
  expect_equal(meta$order, jsonlite::unbox("C"))
  expect_equal(meta$dimension_separator, jsonlite::unbox("."))
})

test_that("create_zarray_meta accepts '/' dimension_separator", {
  dtype <- Dtype$new("<f8")
  meta <- create_zarray_meta(
    shape = c(10), chunks = c(5), dtype = dtype,
    compressor = NA, fill_value = 0, order = "C",
    filters = NA, dimension_separator = "/"
  )
  expect_equal(meta$dimension_separator, jsonlite::unbox("/"))
})

test_that("create_zarray_meta rejects invalid dimension_separator", {
  dtype <- Dtype$new("<f8")
  expect_error(
    create_zarray_meta(
      shape = c(10), chunks = c(5), dtype = dtype,
      compressor = NA, fill_value = 0, order = "C",
      filters = NA, dimension_separator = "-"
    ),
    "dimension_separator"
  )
})

test_that("create_zarray_meta rejects invalid order", {
  dtype <- Dtype$new("<f8")
  expect_error(
    create_zarray_meta(
      shape = c(10), chunks = c(5), dtype = dtype,
      compressor = NA, fill_value = 0, order = "X",
      filters = NA, dimension_separator = "."
    ),
    "order"
  )
})

test_that("create_zarray_meta validates float fill_value", {
  dtype <- Dtype$new("<f4")
  # Valid fill values for float dtype
  expect_silent(create_zarray_meta(
    shape = c(4), chunks = c(4), dtype = dtype,
    compressor = NA, fill_value = 0, order = "C",
    filters = NA, dimension_separator = "."
  ))
  expect_silent(create_zarray_meta(
    shape = c(4), chunks = c(4), dtype = dtype,
    compressor = NA, fill_value = "NaN", order = "C",
    filters = NA, dimension_separator = "."
  ))
  expect_silent(create_zarray_meta(
    shape = c(4), chunks = c(4), dtype = dtype,
    compressor = NA, fill_value = "Infinity", order = "C",
    filters = NA, dimension_separator = "."
  ))
  expect_silent(create_zarray_meta(
    shape = c(4), chunks = c(4), dtype = dtype,
    compressor = NA, fill_value = "-Infinity", order = "C",
    filters = NA, dimension_separator = "."
  ))
})
