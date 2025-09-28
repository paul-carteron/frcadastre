test_that("idu_get_name returns requested location columns for both", {
  idus <- c("721870000A0001", "721870000A0002")
  res <- idu_get_name(idus, loc = "both", cog_field = "NCC")

  expect_true(all(c("idu", "reg_name", "dep_name", "com_name") %in% names(res)))
  expect_equal(nrow(res), length(idus))
})

test_that("idu_get_name returns only region names when loc='reg'", {
  idus <- c("721870000A0001")
  res <- idu_get_name(idus, loc = "reg", cog_field = "NCC")

  expect_true(all(c("idu", "reg_name") %in% names(res)))
  expect_false("dep_name" %in% names(res))
  expect_false("com_name" %in% names(res))
})

test_that("idu_get_name returns only department names when loc='dep'", {
  idus <- c("721870000A0001")
  res <- idu_get_name(idus, loc = "dep", cog_field = "NCC")

  expect_true(all(c("idu", "dep_name") %in% names(res)))
  expect_false("reg_name" %in% names(res))
  expect_false("com_name" %in% names(res))
})

test_that("idu_get_name returns only commune names when loc='com'", {
  idus <- c("721870000A0001")
  res <- idu_get_name(idus, loc = "com", cog_field = "NCC")

  expect_true(all(c("idu", "com_name") %in% names(res)))
  expect_false("reg_name" %in% names(res))
  expect_false("dep_name" %in% names(res))
})

test_that("idu_get_name stops for invalid IDUs", {
  expect_error(
    idu_get_name(c("invalidIDU")),
    "Invalid IDU"
  )
})
