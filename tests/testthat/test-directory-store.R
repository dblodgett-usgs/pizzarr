library(pizzarr)

test_that("Zarr DirectoryStore, can listdir", {
  store <- DirectoryStore$new(tempdir())
  store$set_item("hello/there/a", c(0xbeef))
  store$set_item("hello/there/b", c(0xbeef))
  store$set_item("hello/world/a", c(0xdead))
  store$set_item("hello/world/b", c(0xdead))

  expect_true("hello" %in% store$listdir())
  expect_true("there" %in% store$listdir("hello"))
  expect_true("world" %in% store$listdir("hello"))
  expect_true("a" %in% store$listdir("hello/there"))
  expect_true("b" %in% store$listdir("hello/there"))
})

test_that("Zarr DirectoryStore, can rmdir", {
  store <- DirectoryStore$new(tempdir())
  store$set_item("hello/there/a", c(0xbeef))
  store$set_item("hello/there/b", c(0xbeef))
  store$set_item("hello/world/a", c(0xdead))
  store$set_item("hello/world/b", c(0xdead))

  store$rmdir("hello/there")
  f <- function() store$listdir("hello/there")
  expect_error(f())
  expect_true("world" %in% store$listdir("hello"))
  expect_false("there" %in% store$listdir("hello"))

  store$rmdir("hello")
  expect_false("hello" %in% store$listdir())
})

test_that("DirectoryStore get_item on missing key raises error", {
  store <- DirectoryStore$new(tempfile())
  expect_error(store$get_item("nonexistent"))
})

test_that("DirectoryStore is readable, writeable, and listable", {
  store <- DirectoryStore$new(tempfile())
  expect_true(store$is_readable())
  expect_true(store$is_writeable())
  expect_true(store$is_listable())
})

test_that("DirectoryStore set_item overwrites existing key", {
  d <- tempfile()
  store <- DirectoryStore$new(d)
  store$set_item("key", charToRaw("value1"))
  store$set_item("key", charToRaw("value2"))
  expect_equal(rawToChar(store$get_item("key")), "value2")
})

test_that("DirectoryStore contains_item returns FALSE for missing key", {
  store <- DirectoryStore$new(tempfile())
  expect_false(store$contains_item("nonexistent"))
})

test_that("DirectoryStore contains_item returns TRUE for existing key", {
  store <- DirectoryStore$new(tempfile())
  store$set_item("mykey", charToRaw("myvalue"))
  expect_true(store$contains_item("mykey"))
})
