test_that("idu_assert returns TRUE invisibly for valid IDUs", {
  idus <- c("721870000A0001", "971020000B0002")
  expect_invisible(idu_assert(idus))
})

test_that("idu_assert throws an error for a single invalid IDU", {
  expect_error(
    idu_assert("invalidIDU"),
    "Invalid IDU"
  )
})

test_that("idu_assert throws an error for multiple invalid IDUs", {
  idus <- c("721870000A0001", "invalid1", "invalid2")
  expect_error(
    idu_assert(idus),
    "Invalid IDU\\(s\\) detected: invalid1, invalid2"
  )
})

test_that("idu_assert works with vectorized input", {
  idus <- c("721870000A0001", "971020000B0002")
  expect_invisible(idu_assert(idus))
})

test_that("idu_assert handles NA or empty strings", {
  expect_error(idu_assert(c(NA, "")), "Invalid IDU")
})
