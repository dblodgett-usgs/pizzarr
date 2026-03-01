library(pizzarr)

test_that("dtype regexes work for one-digit number of bytes", {
    dtype_parts <- get_dtype_parts("|u4")
    expect_equal(dtype_parts$basic_type, "u")
    expect_equal(dtype_parts$byte_order, "|")
    expect_equal(dtype_parts$num_bytes, 4)
})

test_that("dtype regexes work for multi-digit number of bytes", {
    dtype_parts <- get_dtype_parts("<f99")
    expect_equal(dtype_parts$basic_type, "f")
    expect_equal(dtype_parts$byte_order, "<")
    expect_equal(dtype_parts$num_bytes, 99)
})

test_that("check_dtype_support throws error for non-matched regex", {
    dtype_parts <- get_dtype_parts("bad")

    expect_equal(is_na(dtype_parts), TRUE)

    f <- function() {
        check_dtype_support(dtype_parts)
    }
    expect_error(f())
})

test_that("get_dtype_rtype works", {
    dtype_rtype <- get_dtype_rtype("f")
    expect_equal(dtype_rtype, double())

    dtype_rtype <- get_dtype_rtype("u")
    expect_equal(dtype_rtype, integer())

    dtype_rtype <- get_dtype_rtype("b")
    expect_equal(dtype_rtype, logical())
})

test_that("get_dtype_endianness works", {
    dtype_endianness <- get_dtype_endianness("<f8")
    expect_equal(dtype_endianness, "little")

    dtype_endianness <- get_dtype_endianness(">f8")
    expect_equal(dtype_endianness, "big")

    dtype_endianness <- get_dtype_endianness("|b1")
    expect_equal(dtype_endianness, "nr")
})

test_that("get_dtype_numbytes works", {
    numbytes <- get_dtype_numbytes("<f8")
    expect_equal(numbytes, 8)

    numbytes <- get_dtype_numbytes("|S12")
    expect_equal(numbytes, 12)
})

test_that("get_dtype_signed works", {
    is_signed <- get_dtype_signed("<f8")
    expect_equal(is_signed, TRUE)

   is_signed <- get_dtype_signed("<u4")
    expect_equal(is_signed, FALSE)
})

test_that("get_dtype_from_array works", {
    zarr_dtype <- get_dtype_from_array(array(data=as.double(1:10), dim=c(2, 5)))
    check_dtype_support(get_dtype_parts(zarr_dtype))
    zarr_dtype <- get_dtype_from_array(array(data=as.integer(1:10), dim=c(2, 5)))
    check_dtype_support(get_dtype_parts(zarr_dtype))

    zarr_dtype <- get_dtype_from_array(array(data=as.logical(1, 0, 1, 0), dim=c(2, 2)))
    expect_false(is_na(get_dtype_parts(zarr_dtype)))
})

test_that("get_dtype_asrtype works", {
    astype_func <- get_dtype_asrtype("<f8")
    a <- astype_func(as.integer(1:10))
    expect_equal(typeof(a), "double")

    astype_func <- get_dtype_asrtype("<i4")
    a <- astype_func(as.double(1:10))
    expect_equal(typeof(a), "integer")

    astype_func <- get_dtype_asrtype("|S8")
    a <- astype_func(as.double(1:10))
    expect_equal(typeof(a), "character")
})

# --- Dtype class tests ---

test_that("Dtype class parses float64", {
  d <- Dtype$new("<f8")
  expect_equal(d$byte_order, "little")
  expect_equal(d$basic_type, "f")
  expect_equal(d$num_bytes, 8)
  expect_true(d$is_signed)
  expect_false(d$is_structured)
  expect_false(d$is_object)
})

test_that("Dtype class parses boolean", {
  d <- Dtype$new("|b1")
  expect_equal(d$byte_order, "nr")
  expect_equal(d$basic_type, "b")
  expect_equal(d$num_bytes, 1)
})

test_that("Dtype class parses unsigned int", {
  d <- Dtype$new("|u1")
  expect_equal(d$basic_type, "u")
  expect_equal(d$num_bytes, 1)
  expect_false(d$is_signed)
})

test_that("Dtype class parses object dtype", {
  d <- Dtype$new("|O")
  expect_equal(d$basic_type, "O")
  expect_true(d$is_object)
})

test_that("Dtype get_rtype returns correct R types", {
  expect_equal(Dtype$new("<f4")$get_rtype(), double())
  expect_equal(Dtype$new("<f8")$get_rtype(), double())
  expect_equal(Dtype$new("<i4")$get_rtype(), integer())
  expect_equal(Dtype$new("|b1")$get_rtype(), logical())
  expect_equal(Dtype$new("|u1")$get_rtype(), integer())
  expect_equal(Dtype$new("|S8")$get_rtype(), character())
})

test_that("Dtype get_asrtype returns correct conversion functions", {
  expect_equal(typeof(Dtype$new("<f8")$get_asrtype()(1L)), "double")
  expect_equal(typeof(Dtype$new("<i4")$get_asrtype()(1.5)), "integer")
  expect_equal(typeof(Dtype$new("|b1")$get_asrtype()(1)), "logical")
})

# --- Dtype round-trip through array creation ---

dtype_cases <- list(
  list(dtype = "|b1", data = array(c(TRUE, FALSE, TRUE, FALSE), dim = c(4))),
  list(dtype = "<i2", data = array(c(1L, 2L, 3L, 4L), dim = c(4))),
  list(dtype = "<i4", data = array(c(1L, 2L, 3L, 4L), dim = c(4))),
  list(dtype = "<f4", data = array(c(1.5, 2.5, 3.5, 4.5), dim = c(4))),
  list(dtype = "<f8", data = array(c(1.5, 2.5, 3.5, 4.5), dim = c(4))),
  list(dtype = "|u1", data = array(c(0L, 127L, 255L, 1L), dim = c(4)))
)

for (case in dtype_cases) {
  test_that(paste("dtype array round-trip:", case$dtype), {
    a <- zarr_create(
      shape = dim(case$data), dtype = case$dtype,
      store = MemoryStore$new(), compressor = NA
    )
    a$set_item("...", case$data)
    result <- a$get_item("...")
    expect_equal(result$data, case$data)
  })
}
