library(pizzarr)


test_that("create group", {
    z <- zarr_create_group()

    name <- z$get_name()

    expect_equal(name, "/")
})

test_that("create nested groups", {
    g1 <- zarr_create_group()
    g2 <- g1$create_group("foo")
    g3 <- g2$create_group("bar")

    name <- g3$get_name()
    expect_equal(name, "/foo/bar")
})

test_that("create array within a nested group", {
    g1 <- zarr_create_group()
    g2 <- g1$create_group("foo")
    g3 <- g2$create_group("bar")

    data <- array(data=1:10, dim=c(2, 5))
    a <- g3$create_dataset("baz", data=data, shape=dim(data))

    expect_equal(a$get_shape(), c(2, 5))
    expect_equal(a$get_name(), "/foo/bar/baz")

    zb_sel <- a$get_item(list(zb_slice(0, 2), zb_slice(0, 2)))
    ob_sel <- a$get_item(list(slice(1, 2), slice(1, 2)))

    expect_equal(zb_sel$data, ob_sel$data)
})

test_that("create group with overwrite = TRUE", {
    store <- MemoryStore$new()
    z <- zarr_create_group(store = store, path = "foo")

    z2 <- zarr_create_group(store = store, path = "foo", overwrite = TRUE)

    name <- z$get_name()
    expect_equal(name, "/foo")

    name2 <- z2$get_name()
    expect_equal(name2, "/foo")
})


test_that("open group twice with MemoryStore", {
    store <- MemoryStore$new()
    z <- zarr_open_group(store = store, path = "foo")

    z2 <- zarr_open_group(store = store, path = "foo")

    name <- z$get_name()
    expect_equal(name, "/foo")

    name2 <- z2$get_name()
    expect_equal(name2, "/foo")
})

test_that("open group twice with DirectoryStore", {
    store <- DirectoryStore$new(tempdir())
    z <- zarr_open_group(store = store, path = "foo")

    z2 <- zarr_open_group(store = store, path = "foo")

    name <- z$get_name()
    expect_equal(name, "/foo")

    name2 <- z2$get_name()
    expect_equal(name2, "/foo")
})

# --- contains_item tests ---

test_that("ZarrGroup contains_item finds arrays and subgroups", {
  store <- MemoryStore$new()
  g <- zarr_open_group(store)
  g$create_dataset("arr1", data = array(1:4, dim = c(4)), shape = c(4))
  g$create_group("subgroup")

  expect_true(g$contains_item("arr1"))
  expect_true(g$contains_item("subgroup"))
  expect_false(g$contains_item("nonexistent"))
})

# --- Mixed hierarchy navigation ---

test_that("Group get_item returns correct type for arrays vs groups", {
  store <- MemoryStore$new()
  g <- zarr_open_group(store)
  g$create_dataset("myarray", data = array(1:4, dim = c(4)), shape = c(4))
  g$create_group("mygroup")

  item_a <- g$get_item("myarray")
  expect_s3_class(item_a, "ZarrArray")

  item_g <- g$get_item("mygroup")
  expect_s3_class(item_g, "ZarrGroup")
})

test_that("Group get_item on missing key raises error", {
  store <- MemoryStore$new()
  g <- zarr_open_group(store)
  expect_error(g$get_item("nonexistent"))
})

# --- Group attributes ---

test_that("Group attributes set and get", {
  store <- MemoryStore$new()
  g <- zarr_open_group(store)
  g$get_attrs()$set_item("mykey", "myvalue")
  expect_equal(g$get_attrs()$get_item("mykey"), "myvalue")
})

test_that("Group attributes round-trip through reopen", {
  store <- MemoryStore$new()
  g <- zarr_open_group(store)
  g$get_attrs()$set_item("mykey", "myvalue")

  g2 <- zarr_open_group(store, mode = "r")
  expect_equal(g2$get_attrs()$get_item("mykey"), "myvalue")
})

# --- Group properties ---

test_that("Group get_store returns the store", {
  store <- MemoryStore$new()
  g <- zarr_open_group(store)
  expect_identical(g$get_store(), store)
})

test_that("Group get_read_only reflects mode", {
  store <- MemoryStore$new()
  g <- zarr_open_group(store)
  expect_false(g$get_read_only())
})
