test_that("idu_get_attribute() returns a data.frame with single attribute", {
  fake_idu <- "721870000A0001"
  fake_sf <- st_sf(
    idu = fake_idu,
    lieudit = "Bois Joli",
    geometry = st_sfc(st_point(c(1, 1)), crs = 4326)
  )

  with_mocked_bindings(
    idu_get_parcelle = function(idu) fake_sf,
    {
      res <- idu_get_attribute(fake_idu, attribute = "lieudit")
      expect_s3_class(res, "data.frame")
      expect_equal(names(res), c("idu", "lieudit"))
      expect_equal(res$lieudit, "Bois Joli")
    }
  )
})


test_that("idu_get_attribute() returns multiple attributes", {
  fake_idu <- "721870000A0001"
  fake_sf <- st_sf(
    idu = fake_idu,
    lieudit = "Grande Prairie",
    com_name = "Le Mans",
    dep_name = "Sarthe",
    geometry = st_sfc(st_point(c(1, 1)), crs = 4326)
  )

  with_mocked_bindings(
    idu_get_parcelle = function(idu) fake_sf,
    {
      res <- idu_get_attribute(fake_idu, attribute = c("lieudit", "com_name"))
      expect_s3_class(res, "data.frame")
      expect_equal(names(res), c("idu", "lieudit", "com_name"))
      expect_equal(res$com_name, "Le Mans")
    }
  )
})


test_that("idu_get_attribute() can return sf when sf_as_result = TRUE", {
  fake_idu <- "721870000A0001"
  fake_sf <- st_sf(
    idu = fake_idu,
    lieudit = "Forêt",
    geometry = st_sfc(st_point(c(2, 2)), crs = 4326)
  )

  with_mocked_bindings(
    idu_get_parcelle = function(idu) fake_sf,
    {
      res <- idu_get_attribute(fake_idu, attribute = "lieudit", sf_as_result = TRUE)
      expect_s3_class(res, "sf")
      expect_true("geometry" %in% names(res))
    }
  )
})


test_that("idu_get_attribute() errors on invalid attribute", {
  fake_idu <- "721870000A0001"
  fake_sf <- st_sf(
    idu = fake_idu,
    lieudit = "Forêt",
    geometry = st_sfc(st_point(c(2, 2)), crs = 4326)
  )

  with_mocked_bindings(
    idu_get_parcelle = function(idu) fake_sf,
    {
      # Simply check that an error is raised
      expect_error(
        idu_get_attribute(fake_idu, attribute = "invalid_attr")
      )
    }
  )
})



test_that("idu_get_attribute() propagates errors from idu_get_parcelle()", {
  fake_idu <- "721870000A0001"

  with_mocked_bindings(
    idu_get_parcelle = function(idu) stop("Boom!"),
    {
      expect_error(
        idu_get_attribute(fake_idu, attribute = "lieudit"),
        "Boom!"
      )
    }
  )
})
