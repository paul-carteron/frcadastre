test_that("get_etalab_urls() works offline with mocked dependencies", {
  fake_communes <- c("72187", "72181")
  fake_layers <- c("parcelles", "commune")

  # Fake URLs to simulate Etalab server
  fake_urls <- c(
    "https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/72187/raw/pci-72187-parcelles.json.gz",
    "https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/72181/cadastre-72181-commune.json.gz"
  )

  with_mocked_bindings(
    insee_check = function(x) invisible(TRUE),
    check_etalab_data = function(x, type) invisible(TRUE),
    get_etalab_arg_pairs = function(commune, data) {
      data.frame(
        commune = c("72187", "72181"),
        layer   = c("parcelles", "commune"),
        stringsAsFactors = FALSE
      )
    },
    get_etalab_layernames = function(kind) {
      if (kind == "proc") list(proc = "commune")
      else list(raw = "parcelles")
    },
    construct_data_url = function(type, commune, millesime) {
      sprintf("https://cadastre.data.gouv.fr/data/etalab-cadastre/%s/%s",
              millesime, commune)
    },
    detect_urls = function(base, absolute = TRUE) fake_urls,
    {
      urls <- get_etalab_urls(fake_communes, fake_layers, millesime = "latest")
      expect_true(all(urls %in% fake_urls))
      expect_length(urls, 2)
    }
  )
})

test_that("get_etalab_urls() works online with httptest2 mocks", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("get_etalab_urls", {
    # Commune + single layer
    urls1 <- get_etalab_urls("72187", "parcelles")
    expect_true(all(grepl("parcelles", urls1)))

    # Multiple communes + pairwise layers
    urls2 <- get_etalab_urls(
      c("72187", "72181"),
      list(c("parcelles", "lieux_dits"), c("commune"))
    )
    expect_true(all(grepl("(parcelles|lieux_dits|commune)", urls2)))
  })
})
