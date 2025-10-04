test_that("idu_detect_in_df returns both name and position by default", {
  df <- data.frame(
    idu_col = c("721870000A0001", "971020000B0002"),
    other_col = c("A", "B"),
    stringsAsFactors = FALSE
  )
  res <- idu_detect_in_df(df)

  expect_type(res, "list")
  expect_named(res, c("name", "position"))
  expect_equal(res$name, "idu_col")
  expect_equal(res$position, 1)
})

test_that("idu_detect_in_df returns only the name when requested", {
  df <- data.frame(
    idu_col = c("721870000A0001", "971020000B0002"),
    other_col = c("A", "B"),
    stringsAsFactors = FALSE
  )
  res <- idu_detect_in_df(df, output = "name")
  expect_type(res, "character")
  expect_equal(res, "idu_col")
})

test_that("idu_detect_in_df returns only the position when requested", {
  df <- data.frame(
    idu_col = c("721870000A0001", "971020000B0002"),
    other_col = c("A", "B"),
    stringsAsFactors = FALSE
  )
  res <- idu_detect_in_df(df, output = "position")
  expect_type(res, "integer")
  expect_equal(res, 1)
})

test_that("idu_detect_in_df returns NULL if no IDU column is found", {
  df <- data.frame(
    col1 = c("abc", "def"),
    col2 = c("123", "456"),
    stringsAsFactors = FALSE
  )
  expect_message(res <- idu_detect_in_df(df), "No column matches the IDU pattern")
  expect_null(res)
})

test_that("idu_detect_in_df works with vectorized IDU column", {
  df <- data.frame(
    idu_col = c("721870000A0001", "721870000A0002", "721870000A0003"),
    other_col = c("X", "Y", "Z"),
    stringsAsFactors = FALSE
  )
  res <- idu_detect_in_df(df)
  expect_equal(res$name, "idu_col")
  expect_equal(res$position, 1)
})

test_that("idu_detect_in_df throws an error if input is not a data.frame", {
  expect_error(idu_detect_in_df(123), "'df' must be a data.frame")
})
