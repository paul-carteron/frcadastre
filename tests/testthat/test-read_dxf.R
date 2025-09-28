test_that("read_dxf() reads DXF files correctly", {
  # path to DXF files
  dxf1 <- system.file("extdata/1870000A02.DXF", package = "rcadastre")
  dxf2 <- system.file("extdata/1870000C05.DXF", package = "rcadastre")

  # Test reading a single file
  sf1 <- read_dxf(dxf1)
  expect_s3_class(sf1, "sf")
  expect_equal(sf::st_crs(sf1)$epsg, 2154)

  # Test folder read
  dxf_dir <- system.file("extdata", package = "rcadastre")
  sf_all <- read_dxf(dxf_dir)

  # Check if sf
  if (inherits(sf_all, "sf")) {
    expect_s3_class(sf_all, "sf")
    expect_equal(sf::st_crs(sf_all)$epsg, 2154)
  } else {
    expect_type(sf_all, "list")
    expect_true(all(sapply(sf_all, function(x) inherits(x, "sf"))))
    expect_true(all(sapply(sf_all, function(x) sf::st_crs(x)$epsg == 2154)))
  }
})
