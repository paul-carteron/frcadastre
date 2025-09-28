library(testthat)
library(rcadastre)

test_that("get_data_millesimes() rejects invalid site arguments", {
  expect_error(get_data_millesimes("invalid"), "should be one of")
})

test_that("get_data_millesimes() rejects invalid site arguments", {
  expect_error(
    get_data_millesimes("invalid"),
    "'arg' should be one of \"pci\", \"etalab\""
  )
})

test_that("get_data_millesimes() retrieves years online", {
  skip_on_cran()
  skip_if_offline()

  pci_years <- get_data_millesimes("pci")
  etalab_years <- get_data_millesimes("etalab")

  expect_type(pci_years, "character")
  expect_type(etalab_years, "character")
  expect_true(length(pci_years) > 0)
  expect_true(length(etalab_years) > 0)
})
