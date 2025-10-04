test_that("check_etalab_data returns TRUE for valid raw layers", {
  expect_true(check_etalab_data("batiment", "raw"))
  expect_true(check_etalab_data(c("batiment", "parcelle"), "raw"))
})

test_that("check_etalab_data returns TRUE for valid proc layers", {
  expect_true(check_etalab_data("parcelles", "proc"))
  expect_true(check_etalab_data(c("parcelles", "sections"), "proc"))
})

test_that("check_etalab_data accepts multiple types", {
  expect_true(check_etalab_data(c("batiment", "parcelles"), c("raw", "proc")))
})

test_that("check_etalab_data throws an error for invalid layer", {
  expect_error(
    check_etalab_data("fake_layer", "raw"),
    "Invalid layer"
  )
})

test_that("check_etalab_data throws an error for mixed valid and invalid layers", {
  expect_error(
    check_etalab_data(c("batiment", "fake_layer"), "raw"),
    "Invalid layer"
  )
})

test_that("check_etalab_data works with multiple layers at once", {
  layers <- c("batiment", "parcelle")
  expect_true(check_etalab_data(layers, "raw"))

  layers_mixed <- c("parcelles", "communes")
  expect_true(check_etalab_data(layers_mixed, "proc"))
})

test_that("check_etalab_data works with vector of layers across multiple types", {
  layers <- c("batiment", "parcelles")
  expect_true(check_etalab_data(layers, c("raw", "proc")))
})
