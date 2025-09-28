library(sf)

test_that("get_etalab_data_by_idu() works offline with mocked dependencies", {
  # Fake IDU parts
  fake_idu_parts <- data.frame(
    idu = c("72181000AB01", "72181000AB02"),
    insee = c("72187", "72187"),
    stringsAsFactors = FALSE
  )

  # Mock get_etalab to return a fake sf
  fake_sf <- sf::st_sf(id = 1:2, geometry = sf::st_sfc())

  with_mocked_bindings(
    idu_assert = function(idu) TRUE,
    idu_split  = function(idu) fake_idu_parts,
    get_etalab = function(insee_codes, layer) fake_sf,
    {
      res <- get_etalab_data_by_idu(c("72181000AB01","72181000AB02"), layer = "parcelles")
      expect_type(res, "list")
      expect_named(res, c("data", "idu_parts"))
      expect_s3_class(res$data, "sf")
      expect_equal(res$idu_parts, fake_idu_parts)
    }
  )
})

test_that("get_etalab_data_by_idu() respects select_cols argument", {
  fake_idu_parts <- data.frame(
    idu = c("72181000AB01"),
    insee = c("72187"),
    stringsAsFactors = FALSE
  )

  fake_sf <- sf::st_sf(idu = 1:1, cont = 2:2, other = 3:3,
                       geometry = sf::st_sfc())

  with_mocked_bindings(
    idu_assert = function(idu) TRUE,
    idu_split  = function(idu) fake_idu_parts,
    get_etalab = function(insee_codes, layer) fake_sf,
    {
      res <- get_etalab_data_by_idu("72181000AB01", select_cols = c("idu","cont"))
      expect_true(all(colnames(res$data) %in% c("idu","cont")))
      expect_false("other" %in% colnames(res$data))
    }
  )
})

test_that("get_etalab_data_by_idu() works online with httptest2", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("get_etalab_data_by_idu", {
    # Single IDU
    res1 <- get_etalab_data_by_idu("72181000AB01")
    expect_type(res1, "list")
    expect_s3_class(res1$data, "sf")
    expect_true(all(c("data", "idu_parts") %in% names(res1)))

    # Multiple IDUs with layer
    res2 <- get_etalab_data_by_idu(c("72181000AB01","72181000AB02"), layer = "parcelles")
    expect_s3_class(res2$data, "sf")
    expect_equal(nrow(res2$idu_parts), 2)
  })
})
