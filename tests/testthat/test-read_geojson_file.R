library(testthat)
library(rcadastre)

test_that("read_geojson_file() reads gzipped GeoJSON correctly", {
  # Paths to the gzipped GeoJSON files in extdata
  geojson1 <- system.file("extdata/pci-72181-commune.json.gz", package = "rcadastre")
  geojson2 <- system.file("extdata/pci-72187-commune.json.gz", package = "rcadastre")

  # Test reading a single GeoJSON file
  geo1 <- read_geojson_file(geojson1)
  expect_type(geo1, "list")                     # The output should be a list
  expect_true(inherits(geo1$data, "sf"))        # The data element should be an sf object
  expect_true(nzchar(geo1$name))                # The extracted layer name should not be empty

  # Test reading a second GeoJSON file
  geo2 <- read_geojson_file(geojson2)
  expect_type(geo2, "list")
  expect_true(inherits(geo2$data, "sf"))
  expect_true(nzchar(geo2$name))

  # Optional: check that the layer names are different
  expect_true(geo1$name == geo2$name)
})
