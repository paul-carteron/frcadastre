test_that("idu_get_attribute_in_df() works offline with mocked idu_get_attribute()", {
  # Sample input data.frame
  df <- data.frame(
    idu = c("721870000A0001", "721870000A0002"),
    other_col = 1:2,
    stringsAsFactors = FALSE
  )

  # Mock outputs for each attribute
  fake_feuille <- data.frame(
    idu = df$idu,
    feuille = c("721870000A", "721870000B"),
    stringsAsFactors = FALSE
  )
  fake_name <- data.frame(
    idu = df$idu,
    commune = c("Commune1", "Commune2"),
    stringsAsFactors = FALSE
  )
  fake_lieudit <- data.frame(
    idu = df$idu,
    lieudit = c("LieuDit1", "LieuDit2"),
    stringsAsFactors = FALSE
  )
  fake_contenance <- data.frame(
    idu = df$idu,
    contenance = c(1250, 980),
    stringsAsFactors = FALSE
  )

  # Mock idu_get_attribute
  with_mocked_bindings(
    idu_get_attribute = function(idu, attribute, ...) {
      switch(attribute,
             feuille = fake_feuille,
             name = fake_name,
             lieudit = fake_lieudit,
             contenance = fake_contenance)
    },
    idu_detect_in_df = function(df, output = "both") list(name = "idu", pos = 1),
    merge_with_name = function(x, df, ref_x, ref_y, ini_col, fin_col) {
      cbind(x, df[, setdiff(names(df), ref_y), drop = FALSE])
    },
    {
      res <- idu_get_attribute_in_df(df, attributes = c("feuille", "lieudit"))
      expect_s3_class(res, "data.frame")
      expect_true(all(c("idu", "other_col", "feuille", "lieudit") %in% names(res)))
      expect_equal(res$feuille, c("721870000A", "721870000B"))
      expect_equal(res$lieudit, c("LieuDit1", "LieuDit2"))
    }
  )
})

test_that("idu_get_attribute_in_df() works online with real Etalab data [httptest2]", {
  skip_if_not_installed("httptest2")

  df <- data.frame(
    idu = c("721870000A0001", "721870000A0002"),
    other_col = 1:2,
    stringsAsFactors = FALSE
  )

  httptest2::with_mock_dir("idu_get_attribute_in_df", {
    res <- idu_get_attribute_in_df(df, attributes = c("name", "contenance"))
    expect_s3_class(res, "data.frame")
    expect_true(all(c("idu", "other_col", "reg_name", "dep_name", "com_name", "contenance") %in% names(res)))
    expect_true(all(res$contenance > 0))
  })
})
