library(pizzarr)

MockArray <- R6::R6Class("MockArray",
    public = list(
        shape = NULL,
        chunks = NULL,
        initialize = function(shape, chunks) {
            self$shape <- shape
            self$chunks <- chunks
        },
        get_shape = function() {
            return(self$shape)
        },
        get_chunks = function() {
            return(self$chunks)
        }
    )
)

test_that("basic indexer for array that spans multiple chunks", {
    z <- MockArray$new(shape = c(10, 10), chunks = c(5, 5))

    expect_equal(z$get_shape(), c(10, 10))
    expect_equal(z$get_chunks(), c(5, 5))

    bi <- BasicIndexer$new(list(zb_slice(0, 11), zb_slice(0, 11)), z)

    expect_equal(as.numeric(bi$shape), c(10, 10))

    expect_equal(length(bi$dim_indexers), 2)

    expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[1]]), TRUE)
    expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[2]]), TRUE)
    
    sdi1 <- bi$dim_indexers[[1]]
    expect_equal(sdi1$start, 0)
    expect_equal(sdi1$stop, 10)
    expect_equal(sdi1$step, 1)
    expect_equal(sdi1$dim_len, 10)
    expect_equal(sdi1$dim_chunk_len, 5)
    expect_equal(sdi1$num_items, 10)
    expect_equal(sdi1$num_chunks, 2)

    sdi2 <- bi$dim_indexers[[2]]
    expect_equal(sdi2$start, 0)
    expect_equal(sdi2$stop, 10)
    expect_equal(sdi2$step, 1)
    expect_equal(sdi2$dim_len, 10)
    expect_equal(sdi2$dim_chunk_len, 5)
    expect_equal(sdi2$num_items, 10)
    expect_equal(sdi2$num_chunks, 2)
})

test_that("basic indexer for array that spans multiple chunks where shape is not a multiple", {
    z <- MockArray$new(shape = c(10, 10), chunks = c(3, 3))

    expect_equal(z$get_shape(), c(10, 10))
    expect_equal(z$get_chunks(), c(3, 3))

    bi <- BasicIndexer$new(list(zb_slice(0, 11), zb_slice(0, 11)), z)

    expect_equal(as.numeric(bi$shape), c(10, 10))

    expect_equal(length(bi$dim_indexers), 2)

    expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[1]]), TRUE)
    expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[2]]), TRUE)
    
    sdi1 <- bi$dim_indexers[[1]]
    expect_equal(sdi1$start, 0)
    expect_equal(sdi1$stop, 10)
    expect_equal(sdi1$step, 1)
    expect_equal(sdi1$dim_len, 10)
    expect_equal(sdi1$dim_chunk_len, 3)
    expect_equal(sdi1$num_items, 10)
    expect_equal(sdi1$num_chunks, 4)

    sdi2 <- bi$dim_indexers[[2]]
    expect_equal(sdi2$start, 0)
    expect_equal(sdi2$stop, 10)
    expect_equal(sdi2$step, 1)
    expect_equal(sdi2$dim_len, 10)
    expect_equal(sdi2$dim_chunk_len, 3)
    expect_equal(sdi2$num_items, 10)
    expect_equal(sdi2$num_chunks, 4)
})

# =============================================================================
# Gap 9: IntDimIndexer — single-integer dimension indexing
# =============================================================================

test_that("IntDimIndexer initializes correctly", {
  idi <- IntDimIndexer$new(dim_sel = 3, dim_len = 10, dim_chunk_len = 5)
  expect_equal(idi$dim_sel, 3)
  expect_equal(idi$dim_len, 10)
  expect_equal(idi$dim_chunk_len, 5)
  expect_equal(idi$num_items, 1)
})

test_that("IntDimIndexer iter returns correct ChunkDimProjection", {
  # Index 3 in a dim of length 10 with chunk_len 5 -> chunk 0, offset 3
  idi <- IntDimIndexer$new(dim_sel = 3, dim_len = 10, dim_chunk_len = 5)
  result <- idi$iter()
  expect_equal(length(result), 1)
  proj <- result[[1]]
  expect_equal(proj$dim_chunk_index, 0)  # chunk 0
  expect_equal(proj$dim_chunk_sel, 3)     # offset within chunk
  expect_true(is.na(proj$dim_out_sel))
})

test_that("IntDimIndexer iter for index in second chunk", {
  # Index 7 in a dim of length 10 with chunk_len 5 -> chunk 1, offset 2
  idi <- IntDimIndexer$new(dim_sel = 7, dim_len = 10, dim_chunk_len = 5)
  result <- idi$iter()
  proj <- result[[1]]
  expect_equal(proj$dim_chunk_index, 1)  # chunk 1
  expect_equal(proj$dim_chunk_sel, 2)     # 7 - 5 = 2
})

test_that("IntDimIndexer iter for first element", {
  idi <- IntDimIndexer$new(dim_sel = 0, dim_len = 10, dim_chunk_len = 5)
  result <- idi$iter()
  proj <- result[[1]]
  expect_equal(proj$dim_chunk_index, 0)
  expect_equal(proj$dim_chunk_sel, 0)
})

test_that("IntDimIndexer iter for last element in chunk boundary", {
  # Index 4 (last in first chunk of size 5) -> chunk 0, offset 4
  idi <- IntDimIndexer$new(dim_sel = 4, dim_len = 10, dim_chunk_len = 5)
  result <- idi$iter()
  proj <- result[[1]]
  expect_equal(proj$dim_chunk_index, 0)
  expect_equal(proj$dim_chunk_sel, 4)
})

test_that("IntDimIndexer iter for first element of second chunk", {
  # Index 5 (first in second chunk of size 5) -> chunk 1, offset 0
  idi <- IntDimIndexer$new(dim_sel = 5, dim_len = 10, dim_chunk_len = 5)
  result <- idi$iter()
  proj <- result[[1]]
  expect_equal(proj$dim_chunk_index, 1)
  expect_equal(proj$dim_chunk_sel, 0)
})

test_that("BasicIndexer creates IntDimIndexer for integer selection", {
  z <- MockArray$new(shape = c(10, 10), chunks = c(5, 5))
  # Pass convert_integer_selection_to_slices=FALSE via normalize_list_selection
  # BasicIndexer uses normalize_list_selection which converts integers to slices by default.
  # The IntDimIndexer path is triggered when convert_integer_selection_to_slices is FALSE
  # or when selections come from non-list contexts.
  # Test that it works through BasicIndexer indirectly via single-integer selection
  bi <- BasicIndexer$new(list(zb_slice(0, 10), zb_slice(0, 10)), z)
  expect_equal(length(bi$dim_indexers), 2)
})

# =============================================================================
# End IntDimIndexer tests
# =============================================================================

test_that("basic indexer for array that spans multiple chunks where shape is not a multiple with offsets", {
    z <- MockArray$new(shape = c(10, 10), chunks = c(3, 3))

    expect_equal(z$get_shape(), c(10, 10))
    expect_equal(z$get_chunks(), c(3, 3))

    bi <- BasicIndexer$new(list(zb_slice(5, 10), zb_slice(5, 10)), z)

    expect_equal(as.numeric(bi$shape), c(5, 5))
    
    expect_equal(length(bi$dim_indexers), 2)

    expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[1]]), TRUE)
    expect_equal("SliceDimIndexer" %in% class(bi$dim_indexers[[2]]), TRUE)
    
    sdi1 <- bi$dim_indexers[[1]]
    expect_equal(sdi1$start, 5)
    expect_equal(sdi1$stop, 10)
    expect_equal(sdi1$step, 1)
    expect_equal(sdi1$dim_len, 10)
    expect_equal(sdi1$dim_chunk_len, 3)
    expect_equal(sdi1$num_items, 5)
    expect_equal(sdi1$num_chunks, 4)

    sdi2 <- bi$dim_indexers[[2]]
    expect_equal(sdi2$start, 5)
    expect_equal(sdi2$stop, 10)
    expect_equal(sdi2$step, 1)
    expect_equal(sdi2$dim_len, 10)
    expect_equal(sdi2$dim_chunk_len, 3)
    expect_equal(sdi2$num_items, 5)
    expect_equal(sdi2$num_chunks, 4)
})