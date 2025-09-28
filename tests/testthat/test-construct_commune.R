library(testthat)
library(rcadastre)

test_that("construct_commune returns correct path for a single commune", {
  result <- construct_commune("72187")
  expect_equal(result, file.path("72", "72187"))
})

test_that("construct_commune works with multiple communes", {
  result <- construct_commune(c("72187", "75056"))
  expect_equal(result, c(file.path("72", "72187"), file.path("75", "75056")))
})

test_that("construct_commune handles overseas departments (97x)", {
  result <- construct_commune("97101")
  expect_equal(result, file.path("971", "97101"))
})

test_that("construct_commune errors on invalid codes", {
  # We assume insee_check() throws an error for invalid codes
  expect_error(construct_commune("99999"))
})
