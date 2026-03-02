library(pizzarr)

# --- opt_to_env ---

test_that("opt_to_env converts option names to env var names", {
  expect_equal(opt_to_env("pizzarr.http_store_cache_time_seconds"),
               "PIZZARR_HTTP_STORE_CACHE_TIME_SECONDS")
  expect_equal(opt_to_env("pizzarr.parallel_backend"),
               "PIZZARR_PARALLEL_BACKEND")
  expect_equal(opt_to_env("pizzarr.progress_bar"),
               "PIZZARR_PROGRESS_BAR")
})

# --- pizzarr_option_defaults ---

test_that("pizzarr_option_defaults has expected keys and values", {
  expect_true("pizzarr.http_store_cache_time_seconds" %in% names(pizzarr_option_defaults))
  expect_true("pizzarr.parallel_backend" %in% names(pizzarr_option_defaults))
  expect_true("pizzarr.parallel_write_enabled" %in% names(pizzarr_option_defaults))
  expect_true("pizzarr.progress_bar" %in% names(pizzarr_option_defaults))

  expect_equal(pizzarr_option_defaults$pizzarr.http_store_cache_time_seconds, 3600)
  expect_identical(pizzarr_option_defaults$pizzarr.parallel_backend, NA)
  expect_false(pizzarr_option_defaults$pizzarr.parallel_write_enabled)
  expect_false(pizzarr_option_defaults$pizzarr.progress_bar)
})

# --- init_options sets defaults ---

test_that("init_options sets default option values", {
  # save current values
  saved <- options(
    pizzarr.http_store_cache_time_seconds = NULL,
    pizzarr.parallel_backend = NULL,
    pizzarr.parallel_write_enabled = NULL,
    pizzarr.progress_bar = NULL
  )
  on.exit(do.call(options, saved), add = TRUE)

  init_options()

  expect_equal(getOption("pizzarr.http_store_cache_time_seconds"), 3600)
  expect_identical(getOption("pizzarr.parallel_backend"), NA)
  expect_false(getOption("pizzarr.parallel_write_enabled"))
  expect_false(getOption("pizzarr.progress_bar"))
})

test_that("init_options does not overwrite pre-existing options", {
  saved <- options(
    pizzarr.http_store_cache_time_seconds = 999,
    pizzarr.progress_bar = TRUE
  )
  on.exit(do.call(options, saved), add = TRUE)

  init_options()

  expect_equal(getOption("pizzarr.http_store_cache_time_seconds"), 999)
  expect_true(getOption("pizzarr.progress_bar"))
})

# --- init_options reads environment variables ---

test_that("init_options picks up env var for http_store_cache_time_seconds", {
  saved <- options(pizzarr.http_store_cache_time_seconds = NULL)
  on.exit(do.call(options, saved), add = TRUE)
  withr::local_envvar(PIZZARR_HTTP_STORE_CACHE_TIME_SECONDS = "120")

  init_options()

  expect_equal(getOption("pizzarr.http_store_cache_time_seconds"), 120L)
})

test_that("init_options picks up env var for progress_bar", {
  saved <- options(pizzarr.progress_bar = NULL)
  on.exit(do.call(options, saved), add = TRUE)
  withr::local_envvar(PIZZARR_PROGRESS_BAR = "TRUE")

  init_options()

  expect_true(getOption("pizzarr.progress_bar"))
})

test_that("init_options picks up env var for parallel_write_enabled", {
  saved <- options(pizzarr.parallel_write_enabled = NULL)
  on.exit(do.call(options, saved), add = TRUE)
  withr::local_envvar(PIZZARR_PARALLEL_WRITE_ENABLED = "TRUE")

  init_options()

  expect_true(getOption("pizzarr.parallel_write_enabled"))
})

test_that("init_options env var does not override pre-existing option", {
  saved <- options(pizzarr.http_store_cache_time_seconds = 999)
  on.exit(do.call(options, saved), add = TRUE)
  withr::local_envvar(PIZZARR_HTTP_STORE_CACHE_TIME_SECONDS = "120")

  init_options()

  # pre-existing option wins over env var
  expect_equal(getOption("pizzarr.http_store_cache_time_seconds"), 999)
})

# --- from_env converters ---

test_that("from_env converters produce expected types", {
  expect_equal(from_env$PIZZARR_HTTP_STORE_CACHE_TIME_SECONDS("42"), 42L)
  expect_true(from_env$PIZZARR_PROGRESS_BAR("TRUE"))
  expect_false(from_env$PIZZARR_PARALLEL_WRITE_ENABLED("FALSE"))
  expect_equal(from_env$PIZZARR_PARALLEL_BACKEND("future"), "future")
  expect_equal(from_env$PIZZARR_PARALLEL_BACKEND("2"), 2)
  expect_equal(from_env$PIZZARR_PARALLEL_BACKEND("0"), FALSE)
})
