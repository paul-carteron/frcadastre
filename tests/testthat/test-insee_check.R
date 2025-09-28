library(testthat)
library(rcadastre)

test_that("insee_check validates communes correctly", {
  # Commune code known in rcadastre::commune_2025
  expect_invisible(insee_check("72187", scale_as_return = FALSE, verbose = FALSE))

  # With scale_as_return = TRUE, should return "communes"
  expect_equal(insee_check("72187", scale_as_return = TRUE, verbose = FALSE), "communes")
})

test_that("insee_check validates departments correctly", {
  # Department code known in rcadastre::departement_2025
  expect_invisible(insee_check("72", scale_as_return = FALSE, verbose = FALSE))

  # With scale_as_return = TRUE, should return "departements"
  expect_equal(insee_check("72", scale_as_return = TRUE, verbose = FALSE), "departements")
})

test_that("insee_check handles multiple codes", {
  codes <- c("72", "72187")
  expect_equal(
    insee_check(codes, scale_as_return = TRUE, verbose = FALSE),
    c("departements", "communes")
  )
})

test_that("insee_check rejects invalid codes", {
  # Too short
  expect_error(insee_check("7", verbose = FALSE), "Invalid code")

  # Commune not found
  expect_error(insee_check("99999", verbose = FALSE), "Commune '99999' not found")

  # Department not found
  expect_error(insee_check("999", verbose = FALSE), "Department '999' not found")
})
