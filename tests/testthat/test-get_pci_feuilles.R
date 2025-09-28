library(testthat)
library(rcadastre)

test_that("get_pci_feuilles() works online for a real commune", {
  skip_if_offline("cadastre.data.gouv.fr")

  # 72187 example
  feuilles <- get_pci_feuilles("72187", millesime = "latest", format = "edigeo", absolute = FALSE)

  expect_type(feuilles, "character")
  expect_true(length(feuilles) > 0)
  # Sheets must be IDU without extension if absolute = TRUE
  expect_false(any(grepl("\\.tar\\.bz2$", feuilles)))
})

test_that("get_pci_feuilles() works offline with mocked detect_urls", {
  fake_links <- c("edigeo-721870000A01.tar.bz2", "edigeo-721870000B02.tar.bz2")

  with_mocked_bindings(
    detect_urls = function(url, absolute = TRUE) fake_links,
    {
      feuilles_abs <- get_pci_feuilles("72187", absolute = TRUE)
      feuilles_rel <- get_pci_feuilles("72187", absolute = FALSE)

      # Absolute should keep tarballs
      expect_true(all(grepl("\\.tar\\.bz2$", feuilles_abs)))

      # Relative should clean names
      expect_equal(feuilles_rel, c("721870000A01", "721870000B02"))
    }
  )
})
