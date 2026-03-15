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

# =============================================================================
# Gap 7: BloscCodec (V2 and V3) — extensive testing
# =============================================================================

test_that("BloscCodec encode/decode direct raw round-trip", {
  skip_if_not_installed("blosc")
  codec <- BloscCodec$new(cname = "lz4", clevel = 5, shuffle = TRUE)
  # Create a mock zarr array to pass dtype info
  z <- zarr_create(shape = c(8), chunks = c(8), dtype = "<i4",
                   store = MemoryStore$new(), compressor = NA)
  raw_data <- as.raw(rep(c(1:4), each = 4, times = 2))  # 32 bytes = 8 int32s
  encoded <- codec$encode(raw_data, z)
  decoded <- codec$decode(encoded, z)
  expect_equal(decoded, raw_data)
})

test_that("BloscCodec with shuffle=FALSE (noshuffle)", {
  skip_if_not_installed("blosc")
  a <- zarr_create(shape = c(8), chunks = c(8), dtype = "<f8",
                   store = MemoryStore$new(),
                   compressor = BloscCodec$new(cname = "lz4", shuffle = FALSE))
  data <- array(c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8), dim = 8)
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

test_that("BloscCodec with integer shuffle=0 (noshuffle)", {
  skip_if_not_installed("blosc")
  a <- zarr_create(shape = c(8), chunks = c(8), dtype = "<i4",
                   store = MemoryStore$new(),
                   compressor = BloscCodec$new(cname = "lz4", shuffle = 0L))
  data <- array(1:8, dim = 8)
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(as.integer(result$data), 1:8)
})

test_that("BloscCodec with integer shuffle=2 (bitshuffle)", {
  skip_if_not_installed("blosc")
  a <- zarr_create(shape = c(8), chunks = c(8), dtype = "<i4",
                   store = MemoryStore$new(),
                   compressor = BloscCodec$new(cname = "lz4", shuffle = 2L))
  data <- array(1:8, dim = 8)
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(as.integer(result$data), 1:8)
})

test_that("BloscCodec with zstd compressor", {
  skip_if_not_installed("blosc")
  a <- zarr_create(shape = c(100), chunks = c(50), dtype = "<f8",
                   store = MemoryStore$new(),
                   compressor = BloscCodec$new(cname = "zstd", clevel = 3))
  data <- array(as.double(1:100), dim = 100)
  a$set_item("...", data)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

test_that("BloscCodec config round-trip", {
  skip_if_not_installed("blosc")
  codec <- BloscCodec$new(cname = "lz4", clevel = 5, shuffle = TRUE)
  config <- codec$get_config()
  expect_equal(as.character(config$id), "blosc")
  expect_equal(as.character(config$cname), "lz4")
  expect_equal(as.integer(config$clevel), 5L)
  restored <- get_codec(config)
  expect_s3_class(restored, "BloscCodec")
  expect_equal(as.character(restored$cname), "lz4")
})

test_that("BloscCodec requires blosc package", {
  # This test would be hard to run since we can't unload a package.
  # Just verify the constructor works when blosc is installed.
  skip_if_not_installed("blosc")
  codec <- BloscCodec$new()
  expect_s3_class(codec, "BloscCodec")
  expect_equal(codec$cname, "lz4")
  expect_equal(codec$clevel, 5)
})

test_that("BloscCodec 2D array round-trip with multiple chunks", {
  skip_if_not_installed("blosc")
  a <- array(as.double(1:100), dim = c(10, 10))
  z <- zarr_create_array(data = a, shape = dim(a), chunks = c(5, 5),
                         dtype = "<f8",
                         compressor = BloscCodec$new(cname = "lz4"))
  result <- z$get_item("...")
  expect_equal(result$data, a)
})

test_that("BloscCodec warn_if_unk_args warns for unrecognized params", {
  skip_if_not_installed("blosc")
  expect_warning(
    BloscCodec$new(cname = "lz4", unknown_param = TRUE),
    "unrecognized"
  )
})

# --- V3 Blosc round-trip ---

test_that("V3 array with blosc compressor round-trips", {
  skip_if_not_installed("blosc")
  store <- MemoryStore$new()
  z <- zarr_create(shape = c(8L), dtype = "<i4", zarr_format = 3L,
                   compressor = BloscCodec$new(cname = "lz4", clevel = 5),
                   store = store)
  data <- array(c(10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L), dim = 8)
  z$set_item("...", data)
  result <- z$get_item("...")
  expect_equal(as.integer(result$data), c(10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L))
})

# --- Codec base class ---

test_that("Base Codec get_config returns unbox(NA)", {
  codec <- Codec$new()
  config <- codec$get_config()
  expect_true(is.na(config))
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

test_that("BloscCodec get_config includes blocksize and round-trips", {
  skip_if_not_installed("blosc")
  codec <- BloscCodec$new(cname = "lz4", clevel = 5, shuffle = TRUE, blocksize = 512)
  config <- codec$get_config()
  expect_equal(as.integer(config$blocksize), 512L)

  # round-trip through get_codec
  restored <- get_codec(config)
  expect_s3_class(restored, "BloscCodec")
  expect_equal(restored$blocksize, 512L)
})

test_that("BloscCodec NA blocksize normalizes to 0", {
  skip_if_not_installed("blosc")
  codec <- BloscCodec$new(blocksize = NA)
  expect_equal(codec$blocksize, 0L)
  config <- codec$get_config()
  expect_equal(as.integer(config$blocksize), 0L)
})
