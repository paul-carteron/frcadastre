test_that("idu_check returns logical vector when error = FALSE", {
  idus <- c("721870000A0001", "971020000B0002", "invalid")
  res <- idu_check(idus, error = FALSE)

  expect_type(res, "logical")
  expect_length(res, length(idus))
  expect_equal(res, c(TRUE, TRUE, FALSE))
})

test_that("idu_check returns TRUE invisibly when all IDUs are valid", {
  idus <- c("721870000A0001", "971020000B0002")
  expect_invisible(idu_check(idus, error = TRUE))
})

test_that("idu_check throws an error for a single invalid IDU", {
  expect_error(
    idu_check("invalidIDU", error = TRUE),
    "Invalid IDU"
  )
})

test_that("idu_check throws an error listing multiple invalid IDUs", {
  idus <- c("721870000A0001", "invalid1", "invalid2")
  expect_error(
    idu_check(idus, error = TRUE),
    "Invalid IDU\\(s\\) detected: invalid1, invalid2"
  )
})

test_that("idu_check detects NA and empty strings as invalid", {
  x <- c("721870000A0001", NA, "")
  res <- idu_check(x, error = FALSE)
  expect_equal(res, c(TRUE, FALSE, FALSE))
})

test_that("idu_check correctly identifies valid formats", {
  valid_idus <- c(
    "721870000A0001", # typical
    "971020000B0002", # department with B
    "012340000Z9999"  # section letter Z
  )
  res <- idu_check(valid_idus, error = FALSE)
  expect_true(all(res))
})

test_that("idu_check flags invalid formats correctly", {
  invalid_idus <- c(
    "72187",           # too short
    "72X870000A0001",  # invalid char in dep
    "721870000a0001",  # lowercase
    "721870000!0001",  # special char
    "721870000AA001",  # too short (13)
    "721870000AA00011" # too long (15)
  )
  res <- idu_check(invalid_idus, error = FALSE)
  expect_false(any(res))
})

test_that("idu_check handles single valid and single invalid IDU", {
  expect_true(idu_check("721870000A0001", error = FALSE))
  expect_false(idu_check("invalid", error = FALSE))
})

test_that("idu_check invisibly returns TRUE when all valid and error = TRUE", {
  idus <- c("721870000A0001", "971020000B0002")
  expect_invisible(idu_check(idus, error = TRUE))
})

test_that("idu_check throws error when NA or empty and error = TRUE", {
  expect_error(idu_check(c(NA, ""), error = TRUE), "Invalid IDU")
})
