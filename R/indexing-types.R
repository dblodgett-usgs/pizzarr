# Reference: https://github.com/gzuidhof/zarr.js/blob/master/src/core/types.ts

#' @title Chunk Dimension Projection
#'
#' @description
#' A mapping from chunk to output array for a single dimension.
#' Used internally to track which items within a single chunk dimension
#' correspond to which items in the target output array.
#'
#' @format [R6::R6Class] object.
#'
#' @family Indexing classes
#' @keywords internal
ChunkDimProjection <- R6::R6Class("ChunkDimProjection",
  public = list(
    #' @field dim_chunk_index (`integer(1)`)\cr
    #'   Index of chunk.
    dim_chunk_index = NULL,
    #' @field dim_chunk_sel (`integer()` | `list()`)\cr
    #'   Selection of items from chunk array.
    dim_chunk_sel = NULL,
    #' @field dim_out_sel (`integer()` | `list()`)\cr
    #'   Selection of items in target (output) array.
    dim_out_sel = NULL,
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param dim_chunk_index (`integer(1)`)\cr
    #'   Index of chunk.
    #' @param dim_chunk_sel (`integer()` | `list()`)\cr
    #'   Selection of items from chunk array.
    #' @param dim_out_sel (`integer()` | `list()`)\cr
    #'   Selection of items in target (output) array.
    initialize = function(dim_chunk_index, dim_chunk_sel, dim_out_sel) {
      self$dim_chunk_index <- dim_chunk_index
      self$dim_chunk_sel <- dim_chunk_sel
      self$dim_out_sel <- dim_out_sel
    }
  )
)

#' @title Chunk Projection
#'
#' @description
#' A mapping of items from chunk to output array. Can be used to extract
#' items from the chunk array for loading into an output array. Can also be
#' used to extract items from a value array for setting/updating in a chunk
#' array.
#'
#' @format [R6::R6Class] object.
#'
#' @family Indexing classes
#' @keywords internal
ChunkProjection <- R6::R6Class("ChunkProjection",
  public = list(
    #' @field chunk_coords (`integer()`)\cr
    #'   Indices of chunk.
    chunk_coords = NULL,
    #' @field chunk_sel (`list()`)\cr
    #'   Selection of items from chunk array.
    chunk_sel = NULL,
    #' @field out_sel (`list()`)\cr
    #'   Selection of items in target (output) array.
    out_sel = NULL,
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param chunk_coords (`integer()`)\cr
    #'   Indices of chunk.
    #' @param chunk_sel (`list()`)\cr
    #'   Selection of items from chunk array.
    #' @param out_sel (`list()`)\cr
    #'   Selection of items in target (output) array.
    initialize = function(chunk_coords, chunk_sel, out_sel) {
      self$chunk_coords <- chunk_coords
      self$chunk_sel <- chunk_sel
      self$out_sel <- out_sel
    }
  )
)

#' @title Indexer
#'
#' @description
#' Abstract base class for chunk indexers. An indexer maps a user-level
#' array selection to a sequence of [ChunkProjection] objects, each
#' describing which items to read or write within a single chunk.
#' Concrete subclasses include [BasicIndexer] and [OrthogonalIndexer].
#'
#' @format [R6::R6Class] object.
#'
#' @family Indexing classes
#' @keywords internal
Indexer <- R6::R6Class("Indexer",
  public = list(
    #' @field shape (`integer()`)\cr
    #'   Shape of the selection.
    shape = NULL,
    #' @field drop_axes (`integer()` | `NULL`)\cr
    #'   Axes to drop from the result.
    drop_axes = NULL,
    #' @description
    #' Iterate over [ChunkProjection] objects for the selection.
    #'
    #' @return `list()` of [ChunkProjection] objects.
    iter = function() {
      # To be implemented in child class
    }
  )
)

#' @title Dimension Indexer
#'
#' @description
#' Abstract base class for per-dimension chunk indexers. A dimension
#' indexer maps a selection on a single array dimension to a sequence of
#' [ChunkDimProjection] objects. Concrete subclasses include
#' [IntDimIndexer], [SliceDimIndexer], [IntArrayDimIndexer], and
#' [BoolArrayDimIndexer].
#'
#' @format [R6::R6Class] object.
#'
#' @family Indexing classes
#' @keywords internal
DimIndexer <- R6::R6Class("DimIndexer",
  public = list(
    #' @field num_items (`integer(1)`)\cr
    #'   Number of items in the selection.
    num_items = NULL,
    #' @description
    #' Iterate over [ChunkDimProjection] objects for this dimension.
    #'
    #' @return `list()` of [ChunkDimProjection] objects.
    iter = function() {
      # To be implemented in child class
    }
  )
)