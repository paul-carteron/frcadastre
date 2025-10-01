test_that("idu_get_contenance() works offline with mocked Etalab data", {
  # Fake IDU parts
  fake_idu_parts <- data.frame(
    idu = c("721870000A0001", "721870000A0002"),
    insee = c("72187", "72187"),
    stringsAsFactors = FALSE
  )

  # Fake parcels with contenance
  fake_parcelles <- sf::st_sf(
    idu = c("721870000A0001", "721870000A0002"),
    contenance = c(1250, 980),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1))),
    crs = 2154
  )

  # Mock functions
  with_mocked_bindings(
    idu_split = function(idu) fake_idu_parts,
    get_etalab = function(insee, verbose = FALSE) fake_parcelles,
    idu_rename_in_df = function(df, col) df,
    merge_with_name = function(x, df, ref_x, ref_y, ini_col, fin_col) {
      data.frame(idu = df$idu, contenance = df$contenance, stringsAsFactors = FALSE)
    },
    {
      res <- idu_get_contenance(c("721870000A0001", "721870000A0002"))
      expect_s3_class(res, "data.frame")
      expect_true(all(c("idu", "contenance") %in% names(res)))
      expect_equal(res$contenance, c(1250, 980))
    }
  )
})

test_that("idu_get_contenance() works online with real Etalab data [httptest2]", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("idu_get_contenance", {
    idus <- c("721870000A0001", "721870000A0002")
    res <- idu_get_contenance(idus)
    expect_s3_class(res, "data.frame")
    expect_true(all(c("idu", "contenance") %in% names(res)))
    expect_true(all(idus %in% res$idu))
  })
})
