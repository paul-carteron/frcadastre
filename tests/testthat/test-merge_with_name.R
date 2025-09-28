test_that("merge_with_name performs a simple left join and renames column", {
  df1 <- data.frame(id = 1:3, value = letters[1:3])
  df2 <- data.frame(key = 1:3, original = c("A", "B", "C"))

  res <- merge_with_name(df1, df2, "id", "key", "original", "renamed")

  expect_true("renamed" %in% names(res))
  expect_equal(res$renamed, c("A", "B", "C"))
  expect_equal(res$value, df1$value) # original columns preserved
})

test_that("merge_with_name keeps all rows from x even if no match in df", {
  df1 <- data.frame(id = 1:4, value = letters[1:4])
  df2 <- data.frame(key = 1:3, original = c("A", "B", "C"))

  res <- merge_with_name(df1, df2, "id", "key", "original", "renamed")

  expect_equal(nrow(res), nrow(df1))
  expect_true(is.na(res$renamed[4])) # unmatched row gets NA
})

test_that("merge_with_name throws an error if ini_col is missing in df", {
  df1 <- data.frame(id = 1:3)
  df2 <- data.frame(key = 1:3)

  expect_error(
    merge_with_name(df1, df2, "id", "key", "missing_col", "new_col"),
    "Column\\(s\\) 'missing_col' not found"
  )
})

test_that("merge_with_name works with vectorized ini_col input", {
  df1 <- data.frame(id = 1:3)
  df2 <- data.frame(key = 1:3, col1 = letters[1:3], col2 = LETTERS[1:3])

  res1 <- merge_with_name(df1, df2, "id", "key", "col1", "renamed1")
  res2 <- merge_with_name(df1, df2, "id", "key", "col2", "renamed2")

  expect_equal(res1$renamed1, letters[1:3])
  expect_equal(res2$renamed2, LETTERS[1:3])
})
