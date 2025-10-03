test_that("idu_get_parcelle() works with mocked dependencies (happy path)", {
  fake_idu <- c("721870000A0001", "721870000A0002")

  parcelles_sf <- st_sf(
    idu = fake_idu,
    geometry = st_sfc(st_point(c(1,1)), st_point(c(2,2))),
    crs = 4326
  )

  lieudits_sf <- st_sf(
    nom = c("Bois Joli", "Grande Prairie"),
    geometry = st_sfc(st_point(c(1,1)), st_point(c(2,2))),
    crs = 4326
  )

  with_mocked_bindings(
    # Mock Etalab: parcels or lieudits
    get_etalab = function(id, data = "parcelles", verbose = TRUE) {
      if (identical(data, "lieux_dits")) lieudits_sf else parcelles_sf
    },
    # Mock name merging: simply inject lieudit names
    merge_with_name = function(x, y, ref_x, ref_y, ini_col, fin_col, ...) {
      x[[fin_col]] <- c("Bois Joli", "Grande Prairie")
      x
    },
    {
      res <- idu_get_parcelle(fake_idu, with_lieudit = TRUE, with_names = TRUE)
      expect_s3_class(res, "sf")
      expect_true("lieudit" %in% names(res))
      expect_equal(res$lieudit, c("Bois Joli", "Grande Prairie"))
    }
  )
})


test_that("idu_get_parcelle() errors if lieux_dits is not sf", {
  fake_idu <- c("721870000A0001", "721870000A0002")

  parcelles_sf <- st_sf(
    idu = fake_idu,
    geometry = st_sfc(st_point(c(1,1)), st_point(c(2,2))),
    crs = 4326
  )

  with_mocked_bindings(
    # Mock Etalab: returns NULL for lieudits
    get_etalab = function(id, data = "parcelles", verbose = TRUE) {
      if (identical(data, "lieux_dits")) NULL else parcelles_sf
    },
    {
      expect_error(
        idu_get_parcelle(fake_idu, with_lieudit = TRUE, with_names = TRUE),
        "Etalab data must be 'sf' objects"
      )
    }
  )
})


test_that("idu_get_parcelle() stops if get_etalab does not return sf", {
  fake_idu <- "721870000A0001"
  fake_insee <- "72187"

  with_mocked_bindings(
    # Mock IDU validation
    idu_assert = function(idu) TRUE,
    # Mock IDU splitting into components
    idu_split  = function(idu) data.frame(idu = idu, insee = fake_insee, stringsAsFactors = FALSE),
    # Mock Etalab: returns a non-sf object
    get_etalab = function(id, data = "parcelles", verbose = TRUE) list(),
    # Mock renaming, just return input
    idu_rename_in_df = function(df, col) df,
    {
      expect_error(
        idu_get_parcelle(fake_idu),
        "Etalab data must be 'sf' objects."
      )
    }
  )
})
