library(pizzarr)

sample_dir <- tools::R_user_dir("pizzarr")
clean <- !dir.exists(sample_dir)

cache <- pizzarr_sample("dog.ome.zarr")

SlowGettingDirectoryStore <- R6::R6Class("SlowGettingDirectoryStore",
  inherit = DirectoryStore,
  public = list(
    get_item = function(key) {
      # Simulate a slow read such as an HTTP request.
      Sys.sleep(1.0/25)
      return(super$get_item(key))
    }
  )
)

SlowSettingDirectoryStore <- R6::R6Class("SlowSettingDirectoryStore",
  inherit = DirectoryStore,
  public = list(
    set_item = function(key, value) {
      # Simulate a slow write such as an HTTP request.
      Sys.sleep(1.0/25)
      return(super$set_item(key, value))
    }
  )
)

get_dog_arr <- function(slow_setting = FALSE) {
  # The path to the root of the OME-NGFF Zarr store.

  root <- file.path(tempdir(), "dog.ome.zarr")
  
  file.copy(pizzarr_sample("dog.ome.zarr"), dirname(root), 
            recursive = TRUE)
  
  # Open the OME-NGFF as a DirectoryStore.
  if(slow_setting) {
    store <- SlowSettingDirectoryStore$new(root)
  } else {
    store <- SlowGettingDirectoryStore$new(root)
  }

  zarr_arr <- zarr_open(store = store, path = "/0")
  return(zarr_arr)
}

run_parallel_get <- function(num_workers) {
  options(pizzarr.parallel_read_enabled = num_workers)

  zarr_arr <- get_dog_arr()
  arr <- zarr_arr$get_item("...")$data

  options(pizzarr.parallel_read_enabled = FALSE)

  return(sum(arr))
}


run_parallel_set <- function(num_workers) {
  options(pizzarr.parallel_write_enabled = num_workers)

  zarr_arr <- get_dog_arr(slow_setting = TRUE)
  arr <- zarr_arr$get_item("...")$data

  # Set the contents of the array to be twice the original value.
  zarr_arr$set_item("...", arr * 2.0)

  doubled_arr <- zarr_arr$get_item("...")$data

  options(pizzarr.parallel_write_enabled = FALSE)

  return(sum(doubled_arr))
}

cl1 <- parallel::makeCluster(1)
cl2 <- parallel::makeCluster(2)

test_that("can run get_item() and set_item in parallel", {
  
  bench_df <- bench::mark(
    run_parallel_get(cl1),
    run_parallel_get(cl2),
    iterations = 1,
    memory = FALSE,
    filter_gc = FALSE
  )

  expect_equal(unlist(bench_df$result), rep(134538481, 2))
  
  testthat::skip_on_os("windows") 
  # injecting parallel workers this way on windows doesn't work
  expect_equal(bench_df$total_time[[1]] > bench_df$total_time[[2]], TRUE)
  
})

test_that("can run set_item() in parallel", {

  bench_df <- bench::mark(
    run_parallel_set(cl1),
    run_parallel_set(cl2),
    iterations = 1,
    memory = FALSE,
    filter_gc = FALSE
  )

  expect_equal(unlist(bench_df$result), rep(134538481*2.0, 2))
  
  testthat::skip_on_os("windows") 
  # injecting parallel workers this way on windows doesn't work
  expect_equal(bench_df$total_time[[1]] > bench_df$total_time[[2]], TRUE)
  
})

test_that("parse_parallel_option works as expected", {
  expect_equal(parse_parallel_option(cl1), cl1)
  expect_equal(parse_parallel_option("future"), "future")
  expect_equal(parse_parallel_option("0"), FALSE)
  expect_equal(parse_parallel_option(0), FALSE)
  expect_equal(parse_parallel_option("FALSE"), FALSE)
  expect_equal(parse_parallel_option(FALSE), FALSE)
  expect_equal(parse_parallel_option("1"), TRUE)
  expect_equal(parse_parallel_option(1), TRUE)
  expect_equal(parse_parallel_option("TRUE"), TRUE)
  expect_equal(parse_parallel_option(TRUE), TRUE)
  expect_equal(parse_parallel_option("2"), 2)
  expect_equal(parse_parallel_option(2), 2)
})

test_that("is_truthy_parallel_option works as expected", {
  expect_equal(is_truthy_parallel_option(cl1), TRUE)
  expect_equal(is_truthy_parallel_option("future"), TRUE)
  expect_equal(is_truthy_parallel_option(FALSE), FALSE)
  expect_equal(is_truthy_parallel_option(0), FALSE)
  expect_equal(is_truthy_parallel_option(TRUE), TRUE)
  expect_equal(is_truthy_parallel_option(1), TRUE)
  expect_equal(is_truthy_parallel_option(2), TRUE)
})

parallel::stopCluster(cl1)
parallel::stopCluster(cl2)

if(clean) unlink(sample_dir, recursive = TRUE)
