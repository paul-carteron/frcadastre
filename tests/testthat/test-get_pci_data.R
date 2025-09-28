library(testthat)
library(rcadastre)

# Offline test: mock URLs and downloads
test_that("get_pci_data() works offline with mocked download and read functions", {
  # Mock URLs that would normally be returned
  fake_urls <- c("https://fake/edigeo-721870000A01.tar.bz2",
                 "https://fake/edigeo-721870000C05.tar.bz2")

  # Mock download_archives to just return a temp directory with our extdata
  temp_dir <- tempdir()
  fake_extract_path <- file.path(temp_dir, "extdata")
  dir.create(fake_extract_path, showWarnings = FALSE)
  file.copy(system.file("extdata/1870000A02.DXF", package = "rcadastre"),
            fake_extract_path, overwrite = TRUE)
  file.copy(system.file("extdata/1870000C05.DXF", package = "rcadastre"),
            fake_extract_path, overwrite = TRUE)

  # Mock functions
  with_mocked_bindings(
    get_pci_urls = function(x, millesime, format) fake_urls,
    download_archives = function(urls, destfiles, extract_dir, use_subdirs, verbose) list(fake_extract_path),
    {
      # Test DXF format
      sf_data <- get_pci_data("72187", format = "dxf", extract_dir = temp_dir, verbose = FALSE)
      expect_true(inherits(sf_data, "sf") || is.list(sf_data)) # returns sf or list of sf
    }
  )
})

test_that("get_pci_data() works online with real PCI data", {
  skip_on_cran() # skip on CRAN to avoid large downloads

  # 1. Download all sheets for a single commune
  pci_commune <- get_pci_data("72187", format = "dxf", verbose = FALSE)
  expect_true(inherits(pci_commune, "sf"))

  # 2. Download a specific sheet
  pci_sheet <- get_pci_data("72181000AB01", format = "edigeo", verbose = FALSE)
  expect_true(is.list(pci_sheet))
  expect_true(all(sapply(pci_sheet, inherits, "sf")))
})
