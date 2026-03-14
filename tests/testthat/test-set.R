library(pizzarr)

test_that("can set a subset of an array using a NestedArray", {
    a_orig <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create_array(data = a_orig, shape=dim(a_orig), dtype="<f4")

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    s1 <- z$get_item("...")
    expect_equal(a_orig, s1$data)

    a_new <- NestedArray$new(
        data = array(data=c(96, 97, 98, 99), dim=c(2, 2)),
        shape = c(2, 2),
        dtype = "<f4"
    )

    z$set_item(list(slice(1, 2), slice(2, 3)), a_new)

    expected_out <- array(data=1:20, dim=c(2, 10))
    expected_out[1:2, 2:3] <- c(96, 97, 98, 99)
    
    s3 <- z$get_item("...")

    expect_equal(expected_out, s3$data)
})

test_that("set_selection handles scalar value assignment", {
  a <- zarr_create(shape = c(4, 4), chunks = c(2, 2), store = MemoryStore$new(),
                   fill_value = 0, compressor = NA)
  a$set_item("...", 5L)
  result <- a$get_item("...")
  expect_true(all(result$data == 5))
})

test_that("set_selection handles NestedArray value", {
  a <- zarr_create(shape = c(4, 4), chunks = c(2, 2), store = MemoryStore$new(),
                   fill_value = 0, compressor = NA)
  data <- array(1:16, dim = c(4, 4))
  na <- NestedArray$new(data, shape = c(4L, 4L), dtype = a$get_dtype(), order = "C")
  a$set_item("...", na)
  result <- a$get_item("...")
  expect_equal(result$data, data)
})

test_that("set_selection shape mismatch raises error", {
  a <- zarr_create(shape = c(4, 4), chunks = c(2, 2), store = MemoryStore$new(),
                   compressor = NA)
  wrong <- array(1:9, dim = c(3, 3))
  expect_error(a$set_item("...", wrong), "Shape mismatch")
})

test_that("can set a subset of an array using a base R array", {
    a_orig <- array(data=1:20, dim=c(2, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create_array(data = a_orig, shape=dim(a_orig), dtype="<f4")

    expect_equal(z$get_shape(), c(2, 10))
    expect_equal(z$get_chunks(), c(2, 10))

    s1 <- z$get_item("...")
    expect_equal(a_orig, s1$data)

    a_new <- array(data=c(96, 97, 98, 99), dim=c(2, 2))

    z$set_item(list(slice(1, 2), slice(2, 3)), a_new)

    expected_out <- array(data=1:20, dim=c(2, 10))
    expected_out[1:2, 2:3] <- c(96, 97, 98, 99)

    s3 <- z$get_item("...")

    expect_equal(expected_out, s3$data)
})

# --- chunk_setitem paths ---

test_that("chunk_setitem scalar total-replace path", {
  # set_item with scalar covers all chunks; each chunk_setitem call sees is_total_slice=TRUE and is_scalar=TRUE
  a <- zarr_create(
    shape = c(4, 4), chunks = c(2, 2), store = MemoryStore$new(),
    compressor = NA, fill_value = 0
  )
  a$set_item("...", 5L)
  result <- a$get_item("...")
  expect_true(all(result$data == 5))
})

test_that("chunk_setitem partial replace works (partial chunk write)", {
  a <- zarr_create(
    shape = c(4, 4), chunks = c(4, 4), store = MemoryStore$new(),
    compressor = NA, fill_value = 0
  )
  sel <- list(slice(1, 2), slice(1, 2))
  a$set_item(sel, array(c(1L, 2L, 3L, 4L), dim = c(2, 2)))
  result <- a$get_item(sel)
  expect_equal(as.vector(result$data), c(1L, 2L, 3L, 4L))
})

test_that("chunk_setitem partial replace reads existing chunk before overwrite", {
  a <- zarr_create(
    shape = c(4, 4), chunks = c(4, 4), store = MemoryStore$new(),
    compressor = NA, fill_value = 0
  )
  a$set_item("...", array(1:16, dim = c(4, 4)))
  a$set_item(list(slice(1, 2), slice(1, 2)), array(c(99L, 99L, 99L, 99L), dim = c(2, 2)))
  result <- a$get_item("...")
  expect_equal(result$data[1, 1], 99)
  expect_equal(result$data[1, 3], 9L)  # untouched
})

test_that("chunk_setitem partial write into uninitialized chunk succeeds", {
  a <- zarr_create(
    shape = c(4, 4), chunks = c(4, 4), store = MemoryStore$new(),
    compressor = NA, fill_value = 42
  )
  a$set_item(list(slice(1, 2), slice(1, 2)), array(c(1L, 2L, 3L, 4L), dim = c(2, 2)))
  result <- a$get_item(list(slice(1, 2), slice(1, 2)))
  expect_equal(as.vector(result$data), c(1L, 2L, 3L, 4L))
})

test_that("get_chunk_value returns scalar when value is scalar", {
  a <- zarr_create(shape = c(4), chunks = c(2), store = MemoryStore$new(),
                   fill_value = 7, compressor = NA)
  a$set_item("...", 7L)
  result <- a$get_item("...")
  expect_true(all(result$data == 7))
})

test_that("set_item rejects unsupported value types with helpful error", {
  z <- zarr_create(shape = c(4L), chunks = c(4L), dtype = "<f4",
                   fill_value = 0, store = MemoryStore$new())
  expect_error(z$set_item("...", list(1, 2, 3, 4)), "object_codec")
})

test_that("encode_chunk runs without error for compressor=NA", {
  a <- zarr_create(shape = c(2), chunks = c(2), store = MemoryStore$new(), compressor = NA)
  a$set_item("...", array(c(1L, 2L), dim = c(2)))
  result <- a$get_item("...")
  expect_equal(as.vector(result$data), c(1L, 2L))
})
