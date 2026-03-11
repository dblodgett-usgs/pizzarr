library(pizzarr)

# --- Codec encode/decode round-trip tests ---

test_that("ZstdCodec encode/decode round-trip (default level)", {
  codec <- ZstdCodec$new()
  data <- as.raw(sample(0:255, 256, replace = TRUE))
  encoded <- codec$encode(data)
  decoded <- codec$decode(encoded)
  expect_equal(decoded, data)
})

test_that("ZstdCodec encode/decode round-trip (level 5)", {
  codec <- ZstdCodec$new(level = 5)
  data <- as.raw(sample(0:255, 256, replace = TRUE))
  encoded <- codec$encode(data)
  decoded <- codec$decode(encoded)
  expect_equal(decoded, data)
})

test_that("ZlibCodec encode/decode round-trip", {
  codec <- ZlibCodec$new()
  data <- as.raw(sample(0:255, 256, replace = TRUE))
  encoded <- codec$encode(data)
  decoded <- codec$decode(encoded)
  expect_equal(decoded, data)
})

test_that("GzipCodec encode/decode round-trip", {
  codec <- GzipCodec$new()
  data <- as.raw(sample(0:255, 256, replace = TRUE))
  encoded <- codec$encode(data)
  decoded <- codec$decode(encoded)
  expect_equal(decoded, data)
})

test_that("Bz2Codec encode/decode round-trip", {
  codec <- Bz2Codec$new()
  data <- as.raw(sample(0:255, 256, replace = TRUE))
  encoded <- codec$encode(data)
  decoded <- codec$decode(encoded)
  expect_equal(decoded, data)
})

test_that("LzmaCodec encode/decode round-trip", {
  codec <- LzmaCodec$new()
  data <- as.raw(sample(0:255, 256, replace = TRUE))
  encoded <- codec$encode(data)
  decoded <- codec$decode(encoded)
  expect_equal(decoded, data)
})

test_that("BloscCodec encode/decode round-trip", {
  skip_if_not_installed("blosc")
  a <- zarr_create(
    shape = c(8),
    chunks = c(8),
    dtype = "<i4",
    store = MemoryStore$new(),
    compressor = BloscCodec$new()
  )
  data <- array(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L), dim = c(8))
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

# --- Codec config round-trip tests ---

test_that("ZstdCodec config round-trip", {
  codec <- ZstdCodec$new(level = 3)
  config <- codec$get_config()
  expect_equal(config$id, jsonlite::unbox("zstd"))
  expect_equal(config$level, jsonlite::unbox(3))
  restored <- get_codec(config)
  expect_s3_class(restored, "ZstdCodec")
  expect_equal(restored$level, 3)
})

test_that("ZlibCodec config round-trip", {
  codec <- ZlibCodec$new(level = 6)
  config <- codec$get_config()
  expect_equal(config$id, jsonlite::unbox("zlib"))
  restored <- get_codec(config)
  expect_s3_class(restored, "ZlibCodec")
})

test_that("GzipCodec config round-trip", {
  codec <- GzipCodec$new(level = 6)
  config <- codec$get_config()
  expect_equal(config$id, jsonlite::unbox("gzip"))
  restored <- get_codec(config)
  expect_s3_class(restored, "GzipCodec")
})

test_that("Bz2Codec config round-trip", {
  codec <- Bz2Codec$new()
  config <- codec$get_config()
  expect_equal(config$id, jsonlite::unbox("bz2"))
  restored <- get_codec(config)
  expect_s3_class(restored, "Bz2Codec")
})

test_that("LzmaCodec config round-trip", {
  codec <- LzmaCodec$new()
  config <- codec$get_config()
  expect_equal(config$id, jsonlite::unbox("lzma"))
  restored <- get_codec(config)
  expect_s3_class(restored, "LzmaCodec")
})

# --- Edge cases ---

test_that("get_codec with unknown id raises error", {
  expect_error(get_codec(list(id = "nonexistent")), "Unknown codec")
})

test_that("get_default_compressor returns ZstdCodec", {
  codec <- get_default_compressor()
  expect_s3_class(codec, "ZstdCodec")
})

test_that("Base Codec is a no-op pass-through", {
  codec <- Codec$new()
  data <- as.raw(c(1, 2, 3))
  expect_equal(codec$encode(data), data)
  expect_equal(codec$decode(data), data)
})

# --- Full array round-trip with each compressor ---

test_that("Array round-trip with ZlibCodec compressor", {
  a <- zarr_create(
    shape = c(4), chunks = c(4), dtype = "<f8",
    store = MemoryStore$new(), compressor = ZlibCodec$new()
  )
  data <- array(c(1.5, 2.5, 3.5, 4.5), dim = c(4))
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

test_that("Array round-trip with GzipCodec compressor", {
  a <- zarr_create(
    shape = c(4), chunks = c(4), dtype = "<f8",
    store = MemoryStore$new(), compressor = GzipCodec$new()
  )
  data <- array(c(1.5, 2.5, 3.5, 4.5), dim = c(4))
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

test_that("Array round-trip with Bz2Codec compressor", {
  a <- zarr_create(
    shape = c(4), chunks = c(4), dtype = "<f8",
    store = MemoryStore$new(), compressor = Bz2Codec$new()
  )
  data <- array(c(1.5, 2.5, 3.5, 4.5), dim = c(4))
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

test_that("Array round-trip with LzmaCodec compressor", {
  a <- zarr_create(
    shape = c(4), chunks = c(4), dtype = "<f8",
    store = MemoryStore$new(), compressor = LzmaCodec$new()
  )
  data <- array(c(1.5, 2.5, 3.5, 4.5), dim = c(4))
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

test_that("Array round-trip with no compressor", {
  a <- zarr_create(
    shape = c(4), chunks = c(4), dtype = "<f8",
    store = MemoryStore$new(), compressor = NA
  )
  data <- array(c(1.5, 2.5, 3.5, 4.5), dim = c(4))
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})
