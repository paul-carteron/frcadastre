library(testthat)
library(httptest2)
library(sf)

test_that("idu_get_feuille() works offline with mocked get_etalab_data_by_idu", {
  # Fake result from get_etalab_data_by_idu
  fake_sf <- sf::st_sf(
    id = c("721870000A01", "721870000A02"),
    commune = c("72187", "72187"),
    geometry = sf::st_sfc()
  )
  fake_idu_parts <- data.frame(
    idu = c("721870000A01", "721870000A02"),
    insee = c("72187", "72187"),
    stringsAsFactors = FALSE
  )

  fake_res <- list(data = fake_sf, idu_parts = fake_idu_parts)

  with_mocked_bindings(
    get_etalab_data_by_idu = function(idu, layer = NULL, select_cols = NULL) fake_res,
    {
      # Flat vector of feuilles
      feuilles_vec <- idu_get_feuille(c("721870000A01", "721870000A02"))
      expect_type(feuilles_vec, "character")
      expect_equal(feuilles_vec, c("721870000A01", "721870000A02"))

      # Named list of feuilles by INSEE
      feuilles_list <- idu_get_feuille(c("721870000A01", "721870000A02"), result_as_list = TRUE)
      expect_type(feuilles_list, "list")
      expect_named(feuilles_list, "72187")
      expect_equal(feuilles_list$`72187`, c("721870000A01", "721870000A02"))
    }
  )
})

test_that("idu_get_feuille() works online with httptest2", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("idu_get_feuille", {
    # Flat vector
    res_vec <- idu_get_feuille("721870000A01")
    expect_type(res_vec, "character")
    expect_true(length(res_vec) > 0)

    # List output
    res_list <- idu_get_feuille(c("721870000A01", "721870000A02"), result_as_list = TRUE)
    expect_type(res_list, "list")
    expect_true(all(names(res_list) %in% c("72187", "72181")))
  })
})
