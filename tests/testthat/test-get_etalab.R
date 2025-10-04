test_that("get_etalab() works offline with mocked dependencies", {
  fake_arg_pairs <- data.frame(
    commune = "72187",
    layer = "parcelles",
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    check_etalab_data = function(data, type) TRUE,
    get_etalab_arg_pairs = function(id, data, verbose = TRUE) fake_arg_pairs,
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
    get_etalab_arg_pairs = function(id, data, verbose = TRUE) fake_arg_pairs,
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
    # Single layer for a single commune
    parcelles <- get_etalab("72187", "parcelles", verbose = FALSE)
    expect_true(inherits(parcelles, "sf") || is.list(parcelles))

    # Multiple layers for a single commune
    layers <- get_etalab("72187", c("parcelles", "sections"), verbose = FALSE)
    expect_true(is.list(layers))
    expect_true(all(sapply(layers, inherits, "sf")))

    # Multiple communes with the same layer
    same_multi <- get_etalab(c("72187", "72181"), "parcelles", verbose = FALSE)
    # In this case, the result is a single sf object with combined rows
    expect_true(inherits(same_multi, "sf"))

    # Multiple communes with different layers
    notsame_multi <- get_etalab(c("72187", "72181"), list("parcelles", "communes"), verbose = FALSE)
    expect_true(is.list(notsame_multi))
    expect_true(all(sapply(notsame_multi, inherits, "sf")))
  })
})
