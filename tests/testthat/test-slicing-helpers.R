library(pizzarr)

# =============================================================================
# Gap 1: adjust_indices / is_total_slice / is_contiguous_slice / is_positive_slice
# =============================================================================

# --- adjust_indices ---
# Reference: Python slice.indices() semantics

test_that("adjust_indices clamps negative start for positive step", {
  res <- adjust_indices(-20, 5, 1, 10)
  expect_equal(res[1], 0)
  expect_equal(res[3], 1)
})

test_that("adjust_indices clamps negative start for negative step", {
  res <- adjust_indices(-20, 0, -1, 10)
  expect_equal(res[1], -1)
})

test_that("adjust_indices clamps start >= length for positive step", {
  res <- adjust_indices(15, 20, 1, 10)
  expect_equal(res[1], 10)
})

test_that("adjust_indices clamps start >= length for negative step", {
  res <- adjust_indices(15, 0, -1, 10)
  expect_equal(res[1], 9)
})

test_that("adjust_indices clamps negative stop for positive step", {
  res <- adjust_indices(0, -20, 1, 10)
  expect_equal(res[2], 0)
})

test_that("adjust_indices clamps negative stop for negative step", {
  res <- adjust_indices(9, -20, -1, 10)
  expect_equal(res[2], -1)
})

test_that("adjust_indices clamps stop >= length for positive step", {
  res <- adjust_indices(0, 20, 1, 10)
  expect_equal(res[2], 10)
})

test_that("adjust_indices clamps stop >= length for negative step", {
  res <- adjust_indices(9, 20, -1, 10)
  expect_equal(res[2], 9)
})

test_that("adjust_indices computes length for positive step", {
  # start=2, stop=8, step=2 -> length = floor((8-2-1)/2 + 1) = 3
  res <- adjust_indices(2, 8, 2, 10)
  expect_equal(res[4], 3)
})

test_that("adjust_indices computes length for negative step", {
  # start=8, stop=2, step=-1 -> length = floor((8-2-1)/1 + 1) = 6
  res <- adjust_indices(8, 2, -1, 10)
  expect_equal(res[4], 6)
})

test_that("adjust_indices returns 0 length when start >= stop for positive step", {
  res <- adjust_indices(5, 3, 1, 10)
  expect_equal(res[4], 0)
})

test_that("adjust_indices returns 0 length when stop >= start for negative step", {
  res <- adjust_indices(3, 5, -1, 10)
  expect_equal(res[4], 0)
})

test_that("adjust_indices handles full range", {
  res <- adjust_indices(0, 10, 1, 10)
  expect_equal(res, c(0, 10, 1, 10))
})

test_that("adjust_indices handles negative start resolved within range", {
  # start=-3 + length=10 = 7, still in range
  res <- adjust_indices(-3, 10, 1, 10)
  expect_equal(res[1], 7)
  expect_equal(res[4], 3)
})

test_that("adjust_indices handles negative stop resolved within range", {
  # stop=-3 + length=10 = 7
  res <- adjust_indices(0, -3, 1, 10)
  expect_equal(res[2], 7)
  expect_equal(res[4], 7)
})

test_that("adjust_indices with step=3 computes correct length", {
  # start=1, stop=10, step=3 -> items at 1,4,7 -> length=3
  res <- adjust_indices(1, 10, 3, 10)
  expect_equal(res[4], 3)
})

test_that("adjust_indices with negative step=(-2) computes correct length", {
  # start=9, stop=0, step=-2 -> items at 9,7,5,3,1 -> length=5
  res <- adjust_indices(9, 0, -2, 10)
  expect_equal(res[4], 5)
})

# --- is_total_slice ---

test_that("is_total_slice returns TRUE for NULL", {
  expect_true(is_total_slice(NULL, c(10)))
})

test_that("is_total_slice returns TRUE for NA", {
  expect_true(is_total_slice(NA, c(10)))
})

test_that("is_total_slice returns FALSE for numeric scalar", {
  # A numeric scalar gets treated as non-slice
  expect_false(is_total_slice(5, c(10)))
})

# --- is_contiguous_slice ---

test_that("is_contiguous_slice TRUE for step=1", {
  s <- zb_slice(0, 5, 1)
  expect_true(is_contiguous_slice(s))
})

test_that("is_contiguous_slice TRUE for NA step", {
  s <- zb_slice(0, 5)
  expect_true(is_contiguous_slice(s))
})

test_that("is_contiguous_slice FALSE for step=2", {
  s <- zb_slice(0, 5, 2)
  expect_false(is_contiguous_slice(s))
})

test_that("is_contiguous_slice FALSE for non-slice", {
  expect_false(is_contiguous_slice(5))
})

# --- is_positive_slice ---

test_that("is_positive_slice TRUE for step=1", {
  s <- zb_slice(0, 5, 1)
  expect_true(is_positive_slice(s))
})

test_that("is_positive_slice TRUE for NA step", {
  s <- zb_slice(0, 5)
  expect_true(is_positive_slice(s))
})

test_that("is_positive_slice FALSE for negative step", {
  s <- zb_slice(5, 0, -1)
  expect_false(is_positive_slice(s))
})

test_that("is_positive_slice FALSE for non-slice", {
  expect_false(is_positive_slice(5))
})

# --- is_total_slice with list of Slices ---

test_that("is_total_slice TRUE for list of full slices", {
  s1 <- zb_slice(0, 10, 1)
  s2 <- zb_slice(0, 20, 1)
  expect_true(is_total_slice(list(s1, s2), c(10, 20)))
})

test_that("is_total_slice TRUE for list of NA-start/stop slices", {
  s1 <- zb_slice(NA, NA, 1)
  s2 <- zb_slice(NA, NA)
  expect_true(is_total_slice(list(s1, s2), c(10, 20)))
})

test_that("is_total_slice FALSE for partial slice in list", {
  s1 <- zb_slice(0, 5, 1)
  expect_false(is_total_slice(list(s1), c(10)))
})

# --- is_contiguous_selection ---

test_that("is_contiguous_selection TRUE for ellipsis string", {
  expect_true(is_contiguous_selection(list("...")))
})

test_that("is_contiguous_selection TRUE for integer vec", {
  expect_true(is_contiguous_selection(list(c(1L, 2L, 3L))))
})

test_that("is_contiguous_selection TRUE for contiguous slice", {
  s <- zb_slice(0, 5, 1)
  expect_true(is_contiguous_selection(list(s)))
})

test_that("is_contiguous_selection FALSE for non-contiguous slice", {
  s <- zb_slice(0, 5, 2)
  expect_false(is_contiguous_selection(list(s)))
})

# --- is_basic_selection ---

test_that("is_basic_selection TRUE for positive slice", {
  s <- zb_slice(0, 5, 1)
  expect_true(is_basic_selection(s))
})

test_that("is_basic_selection TRUE for numeric scalar", {
  expect_true(is_basic_selection(3))
})

test_that("is_basic_selection FALSE for negative step slice", {
  s <- zb_slice(5, 0, -1)
  expect_false(is_basic_selection(s))
})

# --- Slice$indices edge cases ---

test_that("Slice$indices with step=0 errors", {
  s <- Slice$new(0, 5, 0)
  expect_error(s$indices(10), "step size 0")
})

test_that("Slice$indices with negative step and NA start/stop", {
  # Python: slice(None, None, -1).indices(5) => (4, -1, -1)
  s <- Slice$new(NA, NA, -1)
  res <- s$indices(5)
  expect_equal(res[3], -1)
  expect_equal(res[4], 5)
})

test_that("Slice$indices with negative step and NA stop", {
  # Python: slice(3, None, -1).indices(5) => (3, -1, -1)
  s <- Slice$new(3, NA, -1)
  res <- s$indices(5)
  expect_equal(res[1], 3)
  expect_equal(res[4], 4)
})

test_that("Slice$indices with positive step and NA start", {
  # Python: slice(None, 5, 1).indices(10) => (0, 5, 1)
  s <- Slice$new(NA, 5, 1)
  res <- s$indices(10)
  expect_equal(res[1], 0)
  expect_equal(res[2], 5)
  expect_equal(res[4], 5)
})
