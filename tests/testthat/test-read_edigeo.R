library(testthat)
library(rcadastre)

test_that("read_edigeo() reads THF files correctly", {
  # path to EDIGEO files
  thf1 <- system.file("extdata/E0000A02.THF", package = "rcadastre")
  thf2 <- system.file("extdata/E0000C05.THF", package = "rcadastre")

  # Test reading a single file
  sf_list1 <- read_edigeo(dirname(thf1))
  expect_type(sf_list1, "list")
  expect_true(all(sapply(sf_list1, function(x) inherits(x, "sf"))))

  # Test folder read
  thf_dir <- system.file("extdata", package = "rcadastre")
  sf_list_all <- read_edigeo(thf_dir)

  expect_type(sf_list_all, "list")
  expect_true(all(sapply(sf_list_all, function(x) inherits(x, "sf"))))

  # Check if layer names are not empty
  expect_true(all(nzchar(names(sf_list_all))))
})
