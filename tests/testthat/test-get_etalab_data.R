test_that("get_etalab_data() works offline with mocked dependencies", {
  fake_urls <- c("https://fake/etalab/72187-parcelles.json.gz")

  with_mocked_bindings(
    get_etalab_urls = function(commune, data, millesime) fake_urls,
    download_archives = function(urls, destfiles, extract_dir, verbose) {
      # Simulate the creation of a minimal GeoJSON file
      geojson_file <- file.path(extract_dir, "parcelles.json")
      if (!dir.exists(extract_dir)) dir.create(extract_dir, recursive = TRUE)
      writeLines('{"type":"FeatureCollection","features":[]}', geojson_file)
      list(geojson_file)
    },
    read_geojson = function(files, type = "file") {
      # Return a minimal sf (data.frame with geometry)
      sf::st_sf(id = integer(0), geometry = sf::st_sfc())
    },
    {
      res <- get_etalab_data("72187", "parcelles", verbose = FALSE)
      expect_s3_class(res, "sf")
      expect_named(res, c("id", "geometry"))
    }
  )
})

test_that("get_etalab_data() returns NULL when no URLs", {
  with_mocked_bindings(
    get_etalab_urls = function(...) character(0),
    {
      res <- get_etalab_data("72187", "parcelles", verbose = FALSE)
      expect_null(res)
    }
  )
})

test_that("get_etalab_data() works online with httptest2 mocks", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("get_etalab_data", {
    # Single commune, multiple layers
    res1 <- get_etalab_data("72187", c("parcelles", "lieux_dits"), verbose = FALSE)
    expect_true(inherits(res1, "list") || inherits(res1, "sf"))

    # Multiple communes with pairwise layers
    res2 <- get_etalab_data(
      c("72187", "72181"),
      list(c("parcelles", "lieux_dits"), c("commune")),
      verbose = FALSE
    )
    expect_true(inherits(res2, "list") || inherits(res2, "sf"))
  })
})
