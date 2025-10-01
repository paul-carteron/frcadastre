library(sf)

test_that("idu_get_lieudit() works offline with mocked Etalab data", {
  # Fake parcel and lieudit data
  fake_parcelles <- sf::st_sf(
    idu = c("721870000A0001", "721870000A0002"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1))),
    crs = 2154
  )

  fake_lieudits <- sf::st_sf(
    idu = c("721870000A0001", "721870000A0002"),
    nom = c("LieuDit1", "LieuDit2"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1))),
    crs = 2154
  )

  # Mock functions
  with_mocked_bindings(
    idu_split = function(idu) data.frame(idu = idu, insee = substr(idu, 1, 5), stringsAsFactors = FALSE),
    get_etalab = function(insee, verbose = FALSE) {
      if (identical(insee, "72187")) fake_parcelles else fake_lieudits
    },
    idu_rename_in_df = function(df, col) df,
    merge_with_name = function(x, df, ref_x, ref_y, ini_col, fin_col) {
      data.frame(idu = df$idu, lieudit = df$nom, stringsAsFactors = FALSE)
    },
    {
      res <- idu_get_lieudit(c("721870000A0001", "721870000A0002"))
      expect_s3_class(res, "data.frame")
      expect_true(all(c("idu", "lieudit") %in% names(res)))
      expect_equal(res$lieudit, c("LieuDit1", "LieuDit2"))
    }
  )
})

test_that("idu_get_lieudit() works online with real Etalab data [httptest2]", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("idu_get_lieudit", {
    idus <- c("721870000A0001", "721870000A0002")
    res <- idu_get_lieudit(idus)
    expect_s3_class(res, "data.frame")
    expect_true(all(c("idu", "lieudit") %in% names(res)))
    expect_true(all(idus %in% res$idu))
  })
})
