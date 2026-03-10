# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0/zarr/attrs.py#L7

#' The Zarr Attributes class.
#' @title Attributes Class
#' @docType class
#' @description
#' Class providing access to user attributes on an array or group.
#'
#' @format [R6::R6Class]
#' @rdname Attributes
#' @export
Attributes <- R6::R6Class("Attributes",
  private = list(

    cached_aslist = NULL,

    get_nosync = function() {
      attrs_list <- tryCatch({
        # TODO: use consolidated metadata?
        return(self$store$metadata_class$decode_metadata(self$store$get_item(self$key), auto_unbox = TRUE))
      }, error = function(cond) {
        if(is_key_error(cond)) {
          return(obj_list())
        }
        stop(cond)
      })
      return(attrs_list)
    },
    put_nosync = function(d) {
      self$store$set_item(self$key, self$store$metadata_class$encode_metadata(d, auto_unbox = TRUE))
      if(self$cache) {
        private$cached_aslist <- d
      }
    },
    set_item_nosync = function(item, value) {
      d <- private$get_nosync()
      d[[item]] <- value
      private$put_nosync(d)
    },
    del_item_nosync = function(item) {
      d <- private$get_nosync()
      d[[item]] <- NULL
      private$put_nosync(d)
    }
  ),
  public = list(
    #' @field store Attributes store, already initialized.
    store = NULL,
    #' @field key The key under which the attributes will be stored.
    key = NULL,
    #' @field read_only If True, attributes cannot be modified.
    read_only = NULL,
    #' @field cache If True (default), attributes will be cached locally.
    cache = NULL,
    #' @field synchronizer Only necessary if attributes may be modified from multiple threads or processes.
    synchronizer = NULL,
    #' @description
    #' Create a new Attributes instance.
    #' @param store ([Store])\cr
    #'   Attributes store, already initialized.
    #' @param key (`character(1)`)\cr
    #'   Key to use for attributes (`.zattrs` is default).
    #' @param read_only (`logical(1)`)\cr
    #'   Whether the attributes are read-only.
    #' @param cache (`logical(1)`)\cr
    #'   Whether to cache attributes.
    #' @param synchronizer (`ANY` or `NA`)\cr
    #'   Synchronizer object.
    #' @return An `Attributes` instance.
    initialize = function(store, key = NA, read_only = FALSE, cache = TRUE, synchronizer = NA) {
      if(is_na(key)) {
        key <- ATTRS_KEY
      }
      self$store <- store
      self$key <- key
      self$read_only <- read_only
      self$cache <- cache
      private$cached_aslist <- NA
      
      check_cached <- try_from_zmeta(key, store)
      
      if(cache & !is.null(check_cached)) {
        private$cached_aslist <- check_cached
      }
      
      self$synchronizer <- synchronizer
    },
    #' @description
    #' convert attributes to list
    #' @return `list()`.
    to_list = function() {
      if(self$cache && !is_na(private$cached_aslist)) {
        return(private$cached_aslist)
      }
      d <- private$get_nosync()
      if(self$cache) {
        private$cached_aslist <- d
      }
      return(d)
    },
    #' @description
    #' refresh attributes
    #' @return `NULL` (called for side effects).
    refresh = function() {
      if(self$cache) {
        new_val <- private$get_nosync()
        
        private$cached_aslist <- new_val
      }
    },
    #' @description
    #' check if object contains item
    #' @param x Object to test.
    #' @return `logical(1)`.
    contains = function(x) {
      return(x %in% names(self$to_list()))
    },
    #' @description
    #' get attribute
    #' @param item Character attribute name.
    #' @return The attribute value.
    get_item = function(item) {
      return(self$to_list()[[item]])
    },
    #' @description
    #' set attribute
    #' @param item Character attribute name.
    #' @param value Value to add or update.
    #' @return `NULL` (called for side effects).
    set_item = function(item, value) {
      # TODO: support synchronizer
      private$set_item_nosync(item, value)
    },
    #' @description
    #' delete attribute
    #' @param item Character attribute name.
    #' @return `NULL` (called for side effects).
    del_item = function(item) {
      # TODO: support synchronizer
      private$del_item_nosync(item)
    },
    #' @description
    #' Set cached attributes from V3 embedded metadata.
    #' In V3, attributes are part of zarr.json rather than a separate .zattrs file.
    #' This method pre-populates the cache so the normal .zattrs read path is skipped.
    #' @param attrs_list A named list of attributes from V3 zarr.json.
    #' @return `NULL` (modifies cache in place).
    set_cached_v3_attrs = function(attrs_list) {
      if (is.null(attrs_list) || length(attrs_list) == 0) {
        attrs_list <- obj_list()
      }
      private$cached_aslist <- attrs_list
    }
  )
)
