test_that("get_pci_data() works offline with mocked download and read functions", {
  # Fake URLs
  fake_urls <- c("https://fake/edigeo-721870000A01.tar.bz2",
                 "https://fake/edigeo-721870000C05.tar.bz2")

  # Temp extract path with fake DXF files from extdata
  temp_dir <- tempdir()
  fake_extract_path <- file.path(temp_dir, "extdata")
  dir.create(fake_extract_path, showWarnings = FALSE)
  file.copy(system.file("extdata/1870000A02.DXF", package = "frcadastre"),
            fake_extract_path, overwrite = TRUE)
  file.copy(system.file("extdata/1870000C05.DXF", package = "frcadastre"),
            fake_extract_path, overwrite = TRUE)

  # Mock everything to return our fake paths
  with_mocked_bindings(
    get_pci_urls = function(x, millesime, format) fake_urls,
    download_archives = function(urls, destfiles, extract_dir, use_subdirs, verbose) list(fake_extract_path),
    {
      sf_data <- get_pci_data("72187", format = "dxf", extract_dir = temp_dir, verbose = FALSE)
      expect_true(inherits(sf_data, "sf") || is.list(sf_data))
    }
  )
})

test_that("get_pci_data() works online with real PCI data [httptest2]", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("get_pci_data", {
    # Commune code
    pci_commune <- get_pci_data("72187", format = "dxf", verbose = FALSE)
    expect_true(inherits(pci_commune, "sf"))

    # Sheet code
    pci_sheet <- get_pci_data("72181000AB01", format = "edigeo", verbose = FALSE)
    expect_true(is.list(pci_sheet))
    expect_true(all(sapply(pci_sheet, inherits, "sf")))
  })
})
