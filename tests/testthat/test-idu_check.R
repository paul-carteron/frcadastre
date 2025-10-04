test_that("idu_check returns TRUE for valid IDUs", {
  idus <- c("721870000A0001", "971020000B0002")
  res <- idu_check(idus)

  expect_type(res, "logical")
  expect_length(res, 2)
  expect_true(all(res))
})

test_that("idu_check returns FALSE for invalid IDUs", {
  invalid_idus <- c("72187",           # too short
                    "72X870000A0001",  # invalid character in dep
                    "721870000a0001",  # lowercase letter
                    "721870000!0001",  # special character
                    NA,                # NA value
                    "")                # empty string
  res <- idu_check(invalid_idus)

  expect_type(res, "logical")
  expect_length(res, length(invalid_idus))
  expect_false(any(res))
})

test_that("idu_check works with mixed valid and invalid IDUs", {
  idus <- c("721870000A0001", "invalid", "971020000B0002", "123")
  res <- idu_check(idus)

  expect_equal(res, c(TRUE, FALSE, TRUE, FALSE))
})

test_that("idu_check works with a single valid IDU", {
  res <- idu_check("721870000A0001")
  expect_length(res, 1)
  expect_true(res)
})

test_that("idu_check returns FALSE for NA or empty strings", {
  res <- idu_check(c(NA, ""))
  expect_equal(res, c(FALSE, FALSE))
})
