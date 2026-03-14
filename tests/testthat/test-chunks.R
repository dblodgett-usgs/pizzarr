library(pizzarr)

test_that("no infinite loop", {
    shape <- c(27557749)
    dtype <- "<i4"
    chunks <- NA

    dtype <- normalize_dtype(dtype, object_codec = NA)
    shape <- normalize_shape(shape)
    
    dtype_itemsize <- dtype$num_bytes
    chunks <- normalize_chunks(chunks, shape, dtype_itemsize)
    expect_equal(chunks, c(215295))
})

test_that("can get array that spans multiple chunks", {
    a <- array(data=1:100, dim=c(10, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create_array(data = a, shape=dim(a), chunks=c(5, 5), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(10, 10))
    expect_equal(z$get_chunks(), c(5, 5))

    sel <- z$get_item(list(slice(1, 10), slice(1, 10)))
    expect_equal(a, sel$data)
})

test_that("can get array that spans multiple chunks with ellipsis", {
    a <- array(data=1:100, dim=c(10, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create_array(data = a, shape=dim(a), chunks=c(5, 5), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(10, 10))
    expect_equal(z$get_chunks(), c(5, 5))

    sel <- z$get_item("...")
    expect_equal(a, sel$data)
})

# =============================================================================
# Gap 2: ZarrArray chunk I/O — partial writes, fill values, error paths
# =============================================================================

test_that("chunk_setitem: partial chunk write reads existing data first", {
  # Write full array, then update a subset — exercises partial chunk path
  z <- zarr_create(shape = c(10L), chunks = c(5L), dtype = "<i4",
                   fill_value = 0L)
  z$set_item("...", array(1:10, dim = 10))
  # Partial write to first chunk
  z$set_item(list(slice(1, 3)), array(c(99L, 98L, 97L), dim = 3))
  result <- z$get_item("...")
  expect_equal(as.integer(result$data), c(99L, 98L, 97L, 4L, 5L, 6L, 7L, 8L, 9L, 10L))
})

test_that("chunk_getitem: missing chunk returns fill value", {
  z <- zarr_create(shape = c(10L), chunks = c(5L), dtype = "<f8",
                   fill_value = -999.0)
  # Don't write any data — chunks are missing from store
  result <- z$get_item("...")
  expect_equal(as.double(result$data), rep(-999.0, 10))
})

test_that("chunk_setitem: scalar total-replace creates full chunk from scalar", {
  z <- zarr_create(shape = c(6L), chunks = c(3L), dtype = "<i4",
                   fill_value = 0L)
  z$set_item("...", 42L)
  result <- z$get_item("...")
  expect_equal(as.integer(result$data), rep(42L, 6))
})

test_that("chunk I/O round-trip with multiple chunks (2D)", {
  a <- array(as.double(1:24), dim = c(4, 6))
  z <- zarr_create_array(data = a, shape = dim(a), chunks = c(2, 3),
                         dtype = "<f8")
  result <- z$get_item("...")
  expect_equal(result$data, a)
})

test_that("chunk I/O round-trip with non-aligned chunks (3D)", {
  a <- array(as.double(1:60), dim = c(3, 4, 5))
  z <- zarr_create_array(data = a, shape = dim(a), chunks = c(2, 2, 3),
                         dtype = "<f8")
  result <- z$get_item("...")
  expect_equal(result$data, a)
})

test_that("chunk_setitem: partial write to uninitialized chunk uses fill_value", {
  z <- zarr_create(shape = c(10L), chunks = c(5L), dtype = "<i4",
                   fill_value = 0L)
  z$set_item(list(slice(1, 2)), array(c(100L, 200L), dim = 2))
  result <- z$get_item(list(slice(1, 5)))
  expect_equal(as.integer(result$data), c(100L, 200L, 0L, 0L, 0L))
})

# =============================================================================
# End chunk I/O tests
# =============================================================================

test_that("resize smaller deletes out-of-range chunks from DirectoryStore", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  store <- DirectoryStore$new(tmp)
  z <- zarr_create(shape = c(10L), chunks = c(5L), dtype = "<i4",
                   fill_value = 0L, store = store)
  z$set_item("...", array(1:10, dim = 10))
  # Two chunks: 0, 1
  expect_true(store$contains_item("0"))
  expect_true(store$contains_item("1"))
  z$resize(5L)
  # Chunk 1 should be deleted after resize
  expect_true(store$contains_item("0"))
  expect_false(store$contains_item("1"))
})

test_that("resize smaller deletes out-of-range chunks from MemoryStore", {
  store <- MemoryStore$new()
  z <- zarr_create(shape = c(10L), chunks = c(5L), dtype = "<i4",
                   fill_value = 0L, store = store)
  z$set_item("...", array(1:10, dim = 10))
  z$resize(5L)
  result <- z$get_item("...")
  expect_equal(length(result$data), 5)
  expect_equal(as.integer(result$data), 1:5)
})

test_that("can get array where shape is not a multiple of chunk size", {
    a <- array(data=1:100, dim=c(10, 10))
    #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    # [1,]    1    3    5    7    9   11   13   15   17    19
    # [2,]    2    4    6    8   10   12   14   16   18    20
    z <- zarr_create_array(data = a, shape=dim(a), chunks=c(3, 3), dtype="<f4", fill_value=NA)

    expect_equal(z$get_shape(), c(10, 10))
    expect_equal(z$get_chunks(), c(3, 3))

    sel <- z$get_item("...")
    expect_equal(a, sel$data)
})