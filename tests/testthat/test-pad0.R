test_that("pad0 pads numeric values correctly", {
  res <- pad0(c(1, 23, 456), width = 5)
  expect_equal(res, c("00001", "00023", "00456"))
})

test_that("pad0 pads character values correctly", {
  res <- pad0(c("a", "bc", "def"), width = 4)
  expect_equal(res, c("000a", "00bc", "0def"))
})

test_that("pad0 converts to uppercase when upper = TRUE", {
  res <- pad0(c("ab", "cd"), width = 4, upper = TRUE)
  expect_equal(res, c("00AB", "00CD"))
})

test_that("pad0 handles mixed numeric and character values", {
  res <- pad0(c(1, "b"), width = 3, upper = TRUE)
  expect_equal(res, c("001", "00B"))
})

test_that("pad0 returns character vector of correct length", {
  x <- c(1, 2, 3)
  res <- pad0(x, width = 2)
  expect_type(res, "character")
  expect_length(res, length(x))
})

test_that("pad0 works with vectorized numeric input", {
  vals <- c(1, 23, 456)
  res <- pad0(vals, width = 5)
  expect_equal(res, c("00001", "00023", "00456"))
})

test_that("pad0 works with vectorized character input and uppercase", {
  vals <- c("ab", "cd")
  res <- pad0(vals, width = 4, upper = TRUE)
  expect_equal(res, c("00AB", "00CD"))
})
