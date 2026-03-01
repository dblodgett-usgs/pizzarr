#' @keywords internal
path_to_prefix <- function(path) {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/_storage/store.py#L134
    # assume path already normalized
    if(!is.na(path) && stringr::str_length(path) > 0) {
        prefix <- paste0(path, '/')
    } else {
        prefix <- ""
    }
    return(prefix)
}

#' @keywords internal
contains_array <- function(store, path=NA) {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/storage.py#L91
    # Return True if the store contains an array at the given logical path.
    path <- normalize_storage_path(path)
    prefix <- path_to_prefix(path)
    key <- paste0(prefix, ARRAY_META_KEY)
    ret <- store$contains_item(key)
    return(!is.null(ret) && ret)
}

#' @keywords internal
contains_group <- function(store, path=NA) {
    # Reference: https://github.com/zarr-developers/zarr-python/blob/5dd4a0e6cdc04c6413e14f57f61d389972ea937c/zarr/storage.py#L99
    # Return True if the store contains a group at the given logical path.

    path <- normalize_storage_path(path)
    prefix <- path_to_prefix(path)
    key <- paste0(prefix, GROUP_META_KEY)
    ret <- store$contains_item(key)

    return(!is.null(ret) && ret)
}
