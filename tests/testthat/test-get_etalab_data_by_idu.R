library(sf)

test_that("get_etalab_data_by_idu() works offline with mocked dependencies", {
  # Fake IDU parts
  fake_idu_parts <- data.frame(
    idu = c("72181000AB0001", "72181000AB0002"),
    insee = c("72181", "72181"),
    stringsAsFactors = FALSE
  )

  # Mock get_etalab to return a fake sf
  fake_sf <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1)))
  )

  with_mocked_bindings(
    idu_assert = function(idu) TRUE,
    idu_split  = function(idu) fake_idu_parts,
    get_etalab = function(insee_codes, layer, verbose = TRUE) fake_sf,
    {
      res <- get_etalab_data_by_idu(c("72181000AB0001","72181000AB0002"), layer = "parcelles")
      expect_type(res, "list")
      expect_named(res, c("data", "idu_parts"))
      expect_s3_class(res$data, "sf")
      expect_equal(res$idu_parts, fake_idu_parts)
    }
  )
})

test_that("get_etalab_data_by_idu() respects select_cols argument", {
  # Fake IDU parts
  fake_idu_parts <- data.frame(
    idu = c("72181000AB0001", "72181000AB0002"),
    insee = c("72181", "72181"),
    stringsAsFactors = FALSE
  )

  # Fake sf with columns matching select_cols
  fake_sf <- sf::st_sf(
    idu = c("72181000AB0001", "72181000AB0002"),
    cont = c(10, 20),
    geometry = sf::st_sfc(
      sf::st_point(c(0,0)),
      sf::st_point(c(1,1))
    )
  )

  with_mocked_bindings(
    idu_assert = function(idu) TRUE,
    idu_split  = function(idu) fake_idu_parts,
    get_etalab = function(insee_codes, layer, verbose = TRUE) fake_sf,
    {
      res <- get_etalab_data_by_idu("72181000AB0001", select_cols = c("idu","cont"))

      # Check that data is a sf and contains only selected columns
      expect_s3_class(res$data, "sf")
      expect_true(all(colnames(res$data) %in% c("idu","cont","geometry")))
      expect_false("other" %in% colnames(res$data))

      # Check that idu_parts matches
      expect_equal(res$idu_parts, fake_idu_parts)
    }
  )
})


test_that("get_etalab_data_by_idu() works online with httptest2", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("get_etalab_data_by_idu", {
    # Single IDU
    res1 <- get_etalab_data_by_idu("72181000AB0001", layer = "parcelles", verbose = FALSE)
    expect_type(res1, "list")
    expect_s3_class(res1$data, "sf")
    expect_true(all(c("data", "idu_parts") %in% names(res1)))

    # Multiple IDUs with layer
    res2 <- get_etalab_data_by_idu(c("72181000AB0001","72181000AB0002"), layer = "parcelles", verbose = FALSE)
    expect_s3_class(res2$data, "sf")
    expect_equal(nrow(res2$idu_parts), 2)
  })
})
