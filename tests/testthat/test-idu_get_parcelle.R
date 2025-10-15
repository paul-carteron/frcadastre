test_that("idu_get_parcelle() works offline with mocked dependencies", {
  fake_idus <- c("721870000A0001", "721870000A0002")
  fake_parts <- data.frame(
    idu = fake_idus,
    insee = c("72187", "72187"),
    stringsAsFactors = FALSE
  )

  fake_parcelles <- sf::st_sf(
    idu = fake_idus,
    geometry = sf::st_sfc(sf::st_point(c(1,1)), sf::st_point(c(2,2)))
  )

  fake_lieudits <- sf::st_sf(
    idu = fake_idus,
    nom = c("Lieu1", "Lieu2"),
    geometry = sf::st_sfc(sf::st_point(c(1,1)), sf::st_point(c(2,2)))
  )

  fake_names <- data.frame(
    idu = fake_idus,
    parc_nom = c("Parc1", "Parc2"),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    idu_check = function(idu, error = TRUE) TRUE,
    idu_split  = function(idu) fake_parts,
    get_etalab = function(ids, layer = NULL, ...) {
      if (!is.null(layer) && layer == "lieux_dits") fake_lieudits else fake_parcelles
    },
    idu_get_cog = function(idu, ...) fake_names,
    merge_with_name = function(x, y, ref_x, ref_y, ini_col, fin_col, ...) {
      if (!is.null(ini_col) && !is.null(fin_col) && ini_col %in% names(y)) {
        y[[fin_col]] <- y[[ini_col]]
        if (ini_col != fin_col) y[[ini_col]] <- NULL
      }
      if (!ref_y %in% names(y)) y[[ref_y]] <- x[[ref_x]]
      merge(x, y, by.x = ref_x, by.y = ref_y, all.x = TRUE)
    },
    st_join = sf::st_join,
    {
      res <- idu_get_parcelle(fake_idus, with_lieudit = TRUE, with_cog = TRUE)
      expect_s3_class(res, "sf")
      expect_true(all(fake_idus %in% res$idu))
      expect_true(all(c("lieudit", "parc_nom") %in% names(res)))
    }
  )
})

test_that("idu_get_parcelle() works online with httptest2 mocks", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("idu_get_parcelle", {
    # Test avec un IDU unique
    res_single <- idu_get_parcelle("721870000A0001")
    expect_s3_class(res_single, "sf")
    expect_true("idu" %in% names(res_single))

    # Test avec plusieurs IDUs
    res_multi <- idu_get_parcelle(c("721870000A0001", "721870000A0002"))
    expect_s3_class(res_multi, "sf")
    expect_true(all(c("idu") %in% names(res_multi)))

    # Test avec lieudit désactivé
    res_no_lieudit <- idu_get_parcelle(c("721870000A0001"), with_lieudit = FALSE)
    expect_s3_class(res_no_lieudit, "sf")
  })
})
