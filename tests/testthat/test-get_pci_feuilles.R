test_that("get_pci_feuilles() retrieves sheets for a real commune [httptest2]", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("get_pci_feuilles", {
    feuilles <- get_pci_feuilles("72187", millesime = "latest", format = "edigeo", absolute = FALSE)

    expect_type(feuilles, "character")
    expect_true(length(feuilles) > 0)
    expect_false(any(grepl("\\.tar\\.bz2$", feuilles)))
  })
})

test_that("get_pci_feuilles() works offline with mocked detect_urls", {
  fake_links <- c("edigeo-721870000A01.tar.bz2", "edigeo-721870000B02.tar.bz2")

  with_mocked_bindings(
    detect_urls = function(url, absolute = TRUE) fake_links,
    {
      feuilles_abs <- get_pci_feuilles("72187", absolute = TRUE)
      feuilles_rel <- get_pci_feuilles("72187", absolute = FALSE)

      expect_true(all(grepl("\\.tar\\.bz2$", feuilles_abs)))
      expect_equal(feuilles_rel, c("721870000A01", "721870000B02"))
    }
  )
})
