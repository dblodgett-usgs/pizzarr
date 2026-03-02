library(pizzarr)

# --- Helper: create a pre-existing array in a store ---
create_existing_array <- function(store, path = NA) {
  zarr_create(
    shape = c(4), chunks = c(4), dtype = "<f8",
    store = store, path = path, fill_value = 0
  )
}

create_existing_group <- function(store, path = NA) {
  zarr_open_group(store, path = path)
}

# =============================================
# zarr_open_array mode tests
# =============================================

# --- Mode "r" ---

test_that("zarr_open_array mode='r' fails if array doesn't exist", {
  store <- MemoryStore$new()
  expect_error(zarr_open_array(store, mode = "r"))
})

test_that("zarr_open_array mode='r' succeeds on existing array", {
  store <- MemoryStore$new()
  create_existing_array(store)
  a <- zarr_open_array(store, mode = "r")
  expect_s3_class(a, "ZarrArray")
})

test_that("zarr_open_array mode='r' returns read-only array", {
  store <- MemoryStore$new()
  create_existing_array(store)
  a <- zarr_open_array(store, mode = "r")
  expect_true(a$get_read_only())
})

# --- Mode "r+" ---

test_that("zarr_open_array mode='r+' fails if array doesn't exist", {
  store <- MemoryStore$new()
  expect_error(zarr_open_array(store, mode = "r+"))
})

test_that("zarr_open_array mode='r+' succeeds on existing array", {
  store <- MemoryStore$new()
  create_existing_array(store)
  a <- zarr_open_array(store, mode = "r+")
  expect_s3_class(a, "ZarrArray")
  expect_false(a$get_read_only())
})

# --- Mode "a" ---

test_that("zarr_open_array mode='a' creates new array when none exists", {
  store <- MemoryStore$new()
  a <- zarr_open_array(store, mode = "a", shape = c(4), chunks = c(4), dtype = "<f8")
  expect_s3_class(a, "ZarrArray")
  expect_equal(a$get_shape(), c(4))
})

test_that("zarr_open_array mode='a' opens existing array", {
  store <- MemoryStore$new()
  create_existing_array(store)
  a <- zarr_open_array(store, mode = "a", shape = c(4))
  expect_s3_class(a, "ZarrArray")
})

# --- Mode "w" ---

test_that("zarr_open_array mode='w' creates array on fresh MemoryStore", {
  store <- MemoryStore$new()
  a <- zarr_open_array(store, mode = "w", shape = c(8), chunks = c(4), dtype = "<i4")
  expect_s3_class(a, "ZarrArray")
  expect_equal(a$get_shape(), c(8))
})

test_that("zarr_open_array mode='w' overwrites existing array", {
  store <- MemoryStore$new()
  create_existing_array(store)
  a <- zarr_open_array(store, mode = "w", shape = c(8), chunks = c(4), dtype = "<i4")
  expect_s3_class(a, "ZarrArray")
  expect_equal(a$get_shape(), c(8))
})

# --- Mode "w-" / "x" ---

test_that("zarr_open_array mode='w-' fails if array exists", {
  store <- MemoryStore$new()
  create_existing_array(store)
  expect_error(zarr_open_array(store, mode = "w-", shape = c(4)))
})

test_that("zarr_open_array mode='w-' creates new array if none exists", {
  store <- MemoryStore$new()
  a <- zarr_open_array(store, mode = "w-", shape = c(4), chunks = c(4), dtype = "<f8")
  expect_s3_class(a, "ZarrArray")
})

test_that("zarr_open_array mode='x' is alias for 'w-'", {
  store <- MemoryStore$new()
  a <- zarr_open_array(store, mode = "x", shape = c(4), chunks = c(4), dtype = "<f8")
  expect_s3_class(a, "ZarrArray")
})

# =============================================
# zarr_open_group mode tests
# =============================================

test_that("zarr_open_group mode='r' fails if group doesn't exist", {
  store <- MemoryStore$new()
  expect_error(zarr_open_group(store, mode = "r"))
})

test_that("zarr_open_group mode='r' succeeds on existing group", {
  store <- MemoryStore$new()
  create_existing_group(store)
  g <- zarr_open_group(store, mode = "r")
  expect_s3_class(g, "ZarrGroup")
})

test_that("zarr_open_group mode='w' creates group on fresh MemoryStore", {
  store <- MemoryStore$new()
  g <- zarr_open_group(store, mode = "w")
  expect_s3_class(g, "ZarrGroup")
})

test_that("zarr_open_group mode='w' overwrites existing group", {
  store <- MemoryStore$new()
  create_existing_group(store)
  g <- zarr_open_group(store, mode = "w")
  expect_s3_class(g, "ZarrGroup")
})

test_that("zarr_open_group mode='a' creates group when none exists", {
  store <- MemoryStore$new()
  g <- zarr_open_group(store, mode = "a")
  expect_s3_class(g, "ZarrGroup")
})

test_that("zarr_open_group mode='w-' fails if group exists", {
  store <- MemoryStore$new()
  create_existing_group(store)
  expect_error(zarr_open_group(store, mode = "w-"))
})

# =============================================
# zarr_open dispatch tests
# =============================================

test_that("zarr_open with shape arg opens as array", {
  store <- MemoryStore$new()
  result <- zarr_open(store, mode = "w", shape = c(4), chunks = c(4), dtype = "<f8")
  expect_s3_class(result, "ZarrArray")
})

test_that("zarr_open without shape, existing array opens as array", {
  store <- MemoryStore$new()
  create_existing_array(store)
  result <- zarr_open(store, mode = "r")
  expect_s3_class(result, "ZarrArray")
})

test_that("zarr_open without shape, existing group opens as group", {
  store <- MemoryStore$new()
  create_existing_group(store)
  result <- zarr_open(store, mode = "r")
  expect_s3_class(result, "ZarrGroup")
})

test_that("zarr_open mode='r' fails when nothing exists", {
  store <- MemoryStore$new()
  expect_error(zarr_open(store, mode = "r"))
})
