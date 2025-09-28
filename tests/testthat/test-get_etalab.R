test_that("get_etalab() works offline with mocked dependencies", {
  fake_arg_pairs <- data.frame(
    commune = "72187",
    layer = "parcelles",
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    check_etalab_data = function(data, type) TRUE,
    get_etalab_arg_pairs = function(id, data) fake_arg_pairs,
    insee_check = function(communes, scale_as_return = TRUE, verbose = TRUE) "communes",
    read_geojson = function(u, type = "url") {
      sf::st_sf(id = integer(0), geometry = sf::st_sfc())
    },
    {
      res <- get_etalab("72187", "parcelles", verbose = FALSE)
      expect_s3_class(res, "sf")
      expect_named(res, c("id", "geometry"))
    }
  )
})

test_that("get_etalab() returns empty list if read_geojson fails", {
  fake_arg_pairs <- data.frame(
    commune = "72187",
    layer = "parcelles",
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    check_etalab_data = function(data, type) TRUE,
    get_etalab_arg_pairs = function(id, data) fake_arg_pairs,
    insee_check = function(communes, scale_as_return = TRUE, verbose = TRUE) "communes",
    read_geojson = function(u, type = "url") NULL,
    {
      res <- get_etalab("72187", "parcelles", verbose = FALSE)
      expect_true(is.list(res))
      expect_equal(length(res), 0) # aucun sf valide récupéré
    }
  )
})

test_that("get_etalab() works online with httptest2 mocks", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("get_etalab", {
    # Un seul layer pour une commune
    parcelles <- get_etalab("72187", "parcelles", verbose = FALSE)
    expect_true(inherits(parcelles, "sf") || is.list(parcelles))

    # Plusieurs layers pour une commune
    layers <- get_etalab("72187", c("parcelles", "sections"), verbose = FALSE)
    expect_true(is.list(layers))
    expect_true(all(sapply(layers, inherits, "sf")))

    # Plusieurs communes avec même layer
    multi <- get_etalab(c("72187", "72032"), "parcelles", verbose = FALSE)
    expect_true(is.list(multi))
    expect_true(all(sapply(multi, inherits, "sf")))
  })
})
