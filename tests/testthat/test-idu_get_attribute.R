test_that("idu_get_attribute() works offline with mocked dependencies", {
  fake_idus <- c("721870000A0001", "721870000A0002")

  fake_sf <- sf::st_sf(
    idu = fake_idus,
    lieudit = c("Bois du Loup", "La Grande Mare"),
    contenance = c(1200, 2500),
    reg_name = c("Pays de la Loire", "Pays de la Loire"),
    dep_name = c("Sarthe", "Sarthe"),
    com_name = c("Saint-Gervais", "Saint-Gervais"),
    geometry = sf::st_sfc(
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2))
    )
  )

  with_mocked_bindings(
    idu_get_parcelle = function(idu, ...) fake_sf,
    st_drop_geometry = sf::st_drop_geometry,
    {
      # Basic test with default attributes
      res <- idu_get_attribute(fake_idus)
      expect_s3_class(res, "data.frame")
      expect_true(all(c("idu", "lieudit") %in% names(res)))
      expect_false("geometry" %in% names(res))

      # Test with sf_as_result = TRUE
      res_sf <- idu_get_attribute(fake_idus, sf_as_result = TRUE)
      expect_s3_class(res_sf, "sf")
      expect_true("geometry" %in% names(res_sf))

      # Test with multiple attributes
      res_multi <- idu_get_attribute(fake_idus, attribute = c("lieudit", "contenance"))
      expect_s3_class(res_multi, "data.frame")
      expect_true(all(c("idu", "lieudit", "contenance") %in% names(res_multi)))
    }
  )
})


test_that("idu_get_attribute() works online with httptest2 mocks", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("idu_get_attribute", {
    # Single IDU - default behavior
    res_single <- idu_get_attribute("721870000A0001")
    expect_true(is.data.frame(res_single) || inherits(res_single, "sf"))
    expect_true("idu" %in% names(res_single))

    # Multiple IDUs with multiple attributes
    res_multi <- idu_get_attribute(
      c("721870000A0001", "721870000A0002"),
      attribute = c("lieudit", "contenance", "com_name")
    )
    expect_true(is.data.frame(res_multi) || inherits(res_multi, "sf"))
    expect_true(all(c("idu", "lieudit", "contenance", "com_name") %in% names(res_multi)))

    # SF output requested
    res_sf <- idu_get_attribute("721870000A0001", sf_as_result = TRUE)
    expect_s3_class(res_sf, "sf")
    expect_true("geometry" %in% names(res_sf))
  })
})
