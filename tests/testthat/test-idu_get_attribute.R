test_that("idu_get_attribute() works offline with mocked internal functions", {
  idus <- c("721870000A0001", "721870000A0002")

  # Mock outputs for each internal function
  fake_name <- data.frame(
    idu = idus,
    commune = c("Commune1", "Commune2"),
    stringsAsFactors = FALSE
  )
  fake_lieudit <- data.frame(
    idu = idus,
    lieudit = c("LieuDit1", "LieuDit2"),
    stringsAsFactors = FALSE
  )
  fake_contenance <- data.frame(
    idu = idus,
    contenance = c(1250, 980),
    stringsAsFactors = FALSE
  )

  # Mock functions
  with_mocked_bindings(
    idu_get_name = function(idu, ...) fake_name,
    idu_get_lieudit = function(idu, ...) fake_lieudit,
    idu_get_contenance = function(idu, ...) fake_contenance,
    {
      # Single attribute
      res1 <- idu_get_attribute(idus, attribute = "lieudit")
      expect_s3_class(res1, "data.frame")
      expect_equal(res1$lieudit, c("LieuDit1", "LieuDit2"))

      # Multiple attributes
      res2 <- idu_get_attribute(idus, attribute = c("name", "contenance"))
      expect_s3_class(res2, "data.frame")
      expect_true(all(c("idu", "commune", "contenance") %in% names(res2)))
      expect_equal(res2$contenance, c(1250, 980))
    }
  )
})

test_that("idu_get_attribute() works online with real Etalab data [httptest2]", {
  skip_if_not_installed("httptest2")

  httptest2::with_mock_dir("idu_get_attribute", {
    idus <- c("721870000A0001", "721870000A0002")

    # Single attribute
    res1 <- idu_get_attribute(idus, attribute = "lieudit")
    expect_s3_class(res1, "data.frame")
    expect_true(all(c("idu", "lieudit") %in% names(res1)))

    # Multiple attributes
    res2 <- idu_get_attribute(idus, attribute = c("name", "contenance"))
    expect_s3_class(res2, "data.frame")
    expect_true(all(c("idu", "commune", "contenance") %in% names(res2)))
  })
})
