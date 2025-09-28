test_that("idu_rename_in_df renames the detected IDU column", {
  df <- data.frame(
    parcel_id = c("721870000A0001", "971020000B0002"),
    other_col = c("X", "Y"),
    stringsAsFactors = FALSE
  )

  df_new <- idu_rename_in_df(df, "IDU")

  expect_true("IDU" %in% names(df_new))
  expect_false("parcel_id" %in% names(df_new))
  expect_equal(df_new$other_col, df$other_col) # other columns unchanged
})

test_that("idu_rename_in_df returns original df with warning if no IDU column found", {
  df <- data.frame(
    col1 = c("A", "B"),
    col2 = c("X", "Y"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    df_new <- idu_rename_in_df(df, "IDU"),
    "No IDU column detected"
  )

  expect_equal(df_new, df)
})

test_that("idu_rename_in_df works with vectorized IDU column", {
  df <- data.frame(
    idu_col = c("721870000A0001", "721870000A0002", "721870000A0003"),
    val = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  df_new <- idu_rename_in_df(df, "IDU")

  expect_equal(names(df_new)[1], "IDU")
  expect_equal(df_new$val, df$val)
})

test_that("idu_rename_in_df keeps other columns intact", {
  df <- data.frame(
    idu_col = c("721870000A0001", "721870000A0002"),
    colA = c("foo", "bar"),
    colB = c(10, 20),
    stringsAsFactors = FALSE
  )

  df_new <- idu_rename_in_df(df, "IDU")

  expect_equal(df_new$colA, df$colA)
  expect_equal(df_new$colB, df$colB)
})
