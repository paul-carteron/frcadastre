test_that("idu_fmt pads numeric values correctly", {
  res <- idu_fmt(c(1, 23, 456), width = 5)
  expect_equal(res, c("00001", "00023", "00456"))
})

test_that("idu_fmt pads character values correctly", {
  res <- idu_fmt(c("a", "bc", "def"), width = 4)
  expect_equal(res, c("000a", "00bc", "0def"))
})

test_that("idu_fmt converts to uppercase when upper = TRUE", {
  res <- idu_fmt(c("ab", "cd"), width = 4, upper = TRUE)
  expect_equal(res, c("00AB", "00CD"))
})

test_that("idu_fmt handles mixed numeric and character values", {
  res <- idu_fmt(c(1, "b"), width = 3, upper = TRUE)
  expect_equal(res, c("001", "00B"))
})

test_that("idu_fmt returns character vector of correct length", {
  x <- c(1, 2, 3)
  res <- idu_fmt(x, width = 2)
  expect_type(res, "character")
  expect_length(res, length(x))
})

test_that("idu_fmt works with vectorized numeric input", {
  vals <- c(1, 23, 456)
  res <- idu_fmt(vals, width = 5)
  expect_equal(res, c("00001", "00023", "00456"))
})

test_that("idu_fmt works with vectorized character input and uppercase", {
  vals <- c("ab", "cd")
  res <- idu_fmt(vals, width = 4, upper = TRUE)
  expect_equal(res, c("00AB", "00CD"))
})
