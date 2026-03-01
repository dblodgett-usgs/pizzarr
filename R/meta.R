

# Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/meta.py#L14
#' @keywords internal
Metadata2 <- R6::R6Class("Metadata2",
    private = list(
        ZARR_FORMAT = 2
    ),
    public = list(
        decode_metadata = function(s, auto_unbox=FALSE) {
            if(is.list(s) || is.null(s)) {
                return(s)
            } else {
                return(try_fromJSON(rawToChar(s), simplifyVector = FALSE))
            }
        },
        encode_metadata = function(meta, auto_unbox=FALSE) {
            return(charToRaw(jsonlite::toJSON(meta, auto_unbox = auto_unbox)))
        },
        decode_array_metadata = function(s) {
            meta <- self$decode_metadata(s)
            if(!is.null(meta)) validate_v2_meta(meta)
            return(meta)
        },
        decode_group_metadata = function(s) {
            meta <- self$decode_metadata(s)
            if(!is.null(meta)) validate_v2_meta(meta)
            return(meta)
        },
        encode_array_metadata = function(meta) {
            clean_meta <- meta
            clean_meta[['zarr_format']] <- jsonlite::unbox(private$ZARR_FORMAT)
            # TODO: clean up meta even further
            return(self$encode_metadata(clean_meta))
        },
        encode_group_metadata = function(meta = NA) {
            meta <- obj_list()
            meta[['zarr_format']] <- jsonlite::unbox(private$ZARR_FORMAT)
            return(self$encode_metadata(meta))
        }
    )
)

validate_v2_meta <- function(meta) {
  if(meta$zarr_format != 2) stop("unsupported zarr format ", meta$zarr_format)
}

#' @keywords internal
try_from_zmeta <- function(key, store) {
  store$get_consolidated_metadata()$metadata[[key]]
}

try_fromJSON <- function(json, warn_message = "Error parsing json was",
                         simplifyVector = FALSE) {
  out <- tryCatch({
    jsonlite::fromJSON(json, simplifyVector)
  }, error = \(e) {
    if(grepl("NaN", e)) {
      tryCatch({
        jsonlite::fromJSON(gsub("NaN", "null", json), simplifyVector)
      }, error = \(e) {
        warning("\n\n", warn_message, "\n\n", e)
        NULL
      })
    } else {
      warning("\n\n", warn_message, "\n\n", e)
      NULL
    }
  })
}

# TODO: v3 metadata

#' Create a list of zarray metadata.
#' @inheritParams zarr_create
#' @return A list.
#' @keywords internal
create_zarray_meta <- function(shape = NA, chunks = NA, dtype = NA, compressor = NA, fill_value = NA, order = NA, filters = NA, dimension_separator = NA) {
  # Reference: https://zarr.readthedocs.io/en/stable/spec/v2.html#metadata
  if(is.na(dimension_separator)) {
    dimension_separator <- "."
  } else if(!(dimension_separator %in% c(".", "/"))) {
    stop("dimension_separator must be '.' or '/'.")
  }
  if(is_na(compressor)) {
    compressor <- jsonlite::unbox(compressor)
  } else if(!is_na(compressor) && !("id" %in% names(compressor))) {
    stop("compressor must contain an 'id' property when not null.")
  }
  if(is_na(filters)) {
    filters <- jsonlite::unbox(filters)
  }
  if(!(order %in% c("C", "F"))) {
    stop("order must be 'C' or 'F'.")
  }
  is_simple_dtype <- (!dtype$is_structured)
  dtype_str <- dtype$dtype
  if(is_simple_dtype) {
    dtype_byteorder <- dtype$byte_order
    dtype_basictype <- dtype$basic_type
    # Validation occurs in Dtype constructor.

    if(dtype_basictype == "f") {
      if(!is.numeric(fill_value) && !(fill_value %in% c("NaN", "Infinity", "-Infinity"))) {
        stop("fill_value must be NaN, Infinity, or -Infinity when dtype is float")
      }
    }
    if(dtype_basictype == "S" && !is.na(fill_value)) {
      # TODO: validate that fill_value is encoded as an ASCII string using the standard Base64 alphabet.
    }
  } else {
    # TODO: validate structured dtypes
  }

  if(is.null(shape)) {
    shape <-jsonlite::unbox(NA)
  }


  # TODO: validate shape param
  # TODO: validate chunks param
  # TODO: validate filters param
  zarray_meta <- list(
    zarr_format = jsonlite::unbox(2),
    shape = shape,
    chunks = chunks,
    dtype = jsonlite::unbox(dtype_str),
    compressor = compressor,
    fill_value = jsonlite::unbox(fill_value),
    order = jsonlite::unbox(order),
    filters = filters,
    dimension_separator = jsonlite::unbox(dimension_separator)
  )
  return(zarray_meta)
}