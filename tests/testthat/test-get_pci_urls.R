test_that("get_pci_urls() works online with real commune code [httptest2]", {
  skip_if_not_installed("httptest2")

  # Enregistre la requête la première fois, puis rejoue hors-ligne
  httptest2::with_mock_dir("get_pci_urls_commune", {
    urls <- get_pci_urls("72187", millesime = "latest", format = "edigeo")

    # All should be tar.bz2 files
    expect_true(all(grepl("\\.tar\\.bz2$", urls)))

    # Should be absolute URLs starting with cadastre.data.gouv.fr
    expect_true(all(grepl("^https://cadastre\\.data\\.gouv\\.fr/", urls)))

    # At least one feuille should be returned
    expect_gt(length(urls), 0)
  })
})

test_that("get_pci_urls() works online with real commune code", {
  skip_on_cran()
  skip_if_offline()

  urls <- get_pci_urls("72187", millesime = "latest", format = "edigeo")

  # All should be tar.bz2 files
  expect_true(all(grepl("\\.tar\\.bz2$", urls)))

  # Should be absolute URLs starting with cadastre.data.gouv.fr
  expect_true(all(grepl("^https://cadastre\\.data\\.gouv\\.fr/", urls)))

  # At least one feuille should be returned
  expect_gt(length(urls), 0)
})

test_that("get_pci_urls() works offline with mocked detect_urls", {
  fake_links <- c(
    "edigeo-721870000A01.tar.bz2",
    "edigeo-721870000B02.tar.bz2"
  )

  with_mocked_bindings(
    detect_urls = function(url, absolute = TRUE) file.path("https://fake", fake_links),
    get_data_millesimes = function(site) "latest",
    {
      urls <- get_pci_urls("72187", millesime = "latest", format = "edigeo")

      expect_true(all(grepl("^https://fake/", urls)))
      expect_true(all(grepl("\\.tar\\.bz2$", urls)))
      expect_equal(basename(urls), fake_links)
    }
  )
})

test_that("get_pci_urls() works with sheet code directly", {
  url <- get_pci_urls("721870000A01", millesime = "latest", format = "edigeo")

  # Should be a single tarball URL ending with the code
  expect_length(url, 1)
  expect_true(grepl("721870000A01\\.tar\\.bz2$", url))
})
