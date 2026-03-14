library(pizzarr)

# =============================================================================
# Gap 8: v2_config_to_v3_codec / build_v3_codec_pipeline
# =============================================================================

test_that("v2_config_to_v3_codec converts gzip", {
  v2_config <- list(id = jsonlite::unbox("gzip"), level = jsonlite::unbox(5L))
  result <- v2_config_to_v3_codec(v2_config)
  expect_equal(as.character(result$name), "gzip")
  expect_equal(as.integer(result$configuration$level), 5L)
})

test_that("v2_config_to_v3_codec converts zstd", {
  v2_config <- list(id = jsonlite::unbox("zstd"), level = jsonlite::unbox(3L))
  result <- v2_config_to_v3_codec(v2_config)
  expect_equal(as.character(result$name), "zstd")
  expect_equal(as.integer(result$configuration$level), 3L)
  # zstd should get checksum added
  expect_false(is.null(result$configuration$checksum))
})

test_that("v2_config_to_v3_codec converts blosc with shuffle int->string", {
  v2_config <- list(
    id = jsonlite::unbox("blosc"),
    cname = jsonlite::unbox("lz4"),
    clevel = jsonlite::unbox(5L),
    shuffle = jsonlite::unbox(1L)
  )
  result <- v2_config_to_v3_codec(v2_config, dtype = "<f4")
  expect_equal(as.character(result$name), "blosc")
  expect_equal(as.character(result$configuration$shuffle), "shuffle")
  expect_equal(as.integer(result$configuration$typesize), 4L)
  expect_false(is.null(result$configuration$blocksize))
})

test_that("v2_config_to_v3_codec converts blosc shuffle=0 to noshuffle", {
  v2_config <- list(
    id = jsonlite::unbox("blosc"),
    cname = jsonlite::unbox("zstd"),
    clevel = jsonlite::unbox(3L),
    shuffle = jsonlite::unbox(0L)
  )
  result <- v2_config_to_v3_codec(v2_config, dtype = "<i4")
  expect_equal(as.character(result$configuration$shuffle), "noshuffle")
})

test_that("v2_config_to_v3_codec converts blosc shuffle=2 to bitshuffle", {
  v2_config <- list(
    id = jsonlite::unbox("blosc"),
    cname = jsonlite::unbox("lz4"),
    clevel = jsonlite::unbox(5L),
    shuffle = jsonlite::unbox(2L)
  )
  result <- v2_config_to_v3_codec(v2_config, dtype = "<f8")
  expect_equal(as.character(result$configuration$shuffle), "bitshuffle")
  expect_equal(as.integer(result$configuration$typesize), 8L)
})

test_that("v2_config_to_v3_codec errors on unsupported codec", {
  v2_config <- list(id = jsonlite::unbox("unknown_codec"))
  expect_error(v2_config_to_v3_codec(v2_config), "unsupported codec")
})

test_that("v2_config_to_v3_codec converts bz2", {
  v2_config <- list(id = jsonlite::unbox("bz2"), level = jsonlite::unbox(9L))
  result <- v2_config_to_v3_codec(v2_config)
  expect_equal(as.character(result$name), "bz2")
})

test_that("v2_config_to_v3_codec converts lzma", {
  v2_config <- list(id = jsonlite::unbox("lzma"), level = jsonlite::unbox(6L),
                    format = jsonlite::unbox(1L))
  result <- v2_config_to_v3_codec(v2_config)
  expect_equal(as.character(result$name), "lzma")
})

test_that("v2_config_to_v3_codec converts zlib", {
  v2_config <- list(id = jsonlite::unbox("zlib"), level = jsonlite::unbox(1L))
  result <- v2_config_to_v3_codec(v2_config)
  expect_equal(as.character(result$name), "zlib")
})

# --- build_v3_codec_pipeline ---

test_that("build_v3_codec_pipeline with no compressor produces bytes codec only", {
  result <- build_v3_codec_pipeline(NA, NA, "<f8")
  expect_equal(length(result), 1)
  expect_equal(as.character(result[[1]]$name), "bytes")
  expect_equal(as.character(result[[1]]$configuration$endian), "little")
})

test_that("build_v3_codec_pipeline with compressor produces bytes + compressor", {
  compressor <- GzipCodec$new()
  result <- build_v3_codec_pipeline(compressor, NA, "<i4")
  expect_equal(length(result), 2)
  expect_equal(as.character(result[[1]]$name), "bytes")
  expect_equal(as.character(result[[2]]$name), "gzip")
})

test_that("build_v3_codec_pipeline single-byte dtype omits endian", {
  result <- build_v3_codec_pipeline(NA, NA, "|u1")
  expect_equal(length(result), 1)
  expect_equal(as.character(result[[1]]$name), "bytes")
  expect_null(result[[1]]$configuration)
})

test_that("build_v3_codec_pipeline with VLenUtf8 filter uses vlen-utf8 codec", {
  vlen_filter <- VLenUtf8Codec$new()
  result <- build_v3_codec_pipeline(NA, list(vlen_filter), "|O")
  expect_equal(length(result), 1)
  expect_equal(as.character(result[[1]]$name), "vlen-utf8")
})

test_that("build_v3_codec_pipeline with compressor and VLenUtf8", {
  vlen_filter <- VLenUtf8Codec$new()
  compressor <- ZstdCodec$new(level = 1)
  result <- build_v3_codec_pipeline(compressor, list(vlen_filter), "|O")
  expect_equal(length(result), 2)
  expect_equal(as.character(result[[1]]$name), "vlen-utf8")
  expect_equal(as.character(result[[2]]$name), "zstd")
})

# --- v3_codec_to_v2_config ---

test_that("v3_codec_to_v2_config round-trips gzip", {
  v2_config <- v3_codec_to_v2_config("gzip", list(level = 5L))
  expect_equal(as.character(v2_config$id), "gzip")
  expect_equal(as.integer(v2_config$level), 5L)
})

test_that("v3_codec_to_v2_config round-trips zstd (strips checksum)", {
  v2_config <- v3_codec_to_v2_config("zstd", list(level = 3L, checksum = FALSE))
  expect_equal(as.character(v2_config$id), "zstd")
  expect_null(v2_config$checksum)
})
