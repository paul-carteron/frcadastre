test_that("idu_build works with valid dep and com codes", {
  res <- idu_build(
    dep = "72", com = "187",
    prefix = "000", section = "A", numero = "1"
  )
  expect_type(res, "character")
  expect_equal(nchar(res), 14)
  expect_true(grepl("^72187", res))
})

test_that("idu_build vectorized input produces correct length", {
  res <- idu_build(
    dep = c("72", "73"),
    com = c("187", "001"),
    prefix = c("0", "1"), section = c("A", "B"), numero = c("1", "2")
  )
  expect_length(res, 2)
  expect_true(all(nchar(res) == 14))
})

test_that("idu_build throws error when input lengths mismatch", {
  expect_error(
    idu_build(
      dep = "72", com = c("187", "001"),
      prefix = "000", section = "A", numero = "1"
    ),
    "must have the same length"
  )
})

test_that("idu_build throws error for missing required arguments", {
  expect_error(
    idu_build(
      dep = "72", com = "187",
      prefix = "000", section = "A" # missing numero
    ),
    "Missing required argument"
  )

  expect_error(
    idu_build(
      com = "187", prefix = "000", section = "A", numero = "1" # missing dep
    ),
    "Missing required argument"
  )
})

test_that("idu_build throws error for invalid dep or com lengths", {
  expect_error(
    idu_build(
      dep = "7", com = "187", prefix = "000", section = "A", numero = "1"
    ),
    "`dep` must have 2 or 3 characters"
  )

  expect_error(
    idu_build(
      dep = "72", com = "1", prefix = "000", section = "A", numero = "1"
    ),
    "`com` must have 2 or 3 characters"
  )
})

test_that("idu_build uppercases section and zero-pads prefix and numero", {
  res <- idu_build(
    dep = "72", com = "187",
    prefix = "1", section = "b", numero = "7"
  )
  expect_true(grepl("B", res))
  expect_true(grepl("001", res))
  expect_true(grepl("0007$", res))
})

test_that("idu_build returns valid IDUs for multiple records", {
  res <- idu_build(
    dep = c("72", "73"),
    com = c("187", "001"),
    prefix = c("000", "001"),
    section = c("A", "B"),
    numero = c("1", "2")
  )
  expect_length(res, 2)
  expect_true(all(nchar(res) == 14))
})
