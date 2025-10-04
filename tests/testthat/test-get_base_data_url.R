test_that("get_base_data_url returns correct URLs", {
  # Test for 'pci'
  expect_equal(
    get_base_data_url("pci"),
    "https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur"
  )

  # Test for 'etalab'
  expect_equal(
    get_base_data_url("etalab"),
    "https://cadastre.data.gouv.fr/data/etalab-cadastre"
  )

  # Test that value is character
  expect_type(get_base_data_url("pci"), "character")
})

test_that("get_base_data_url handles invalid input", {
  # Test that the function throws an error for an invalid argument
  expect_error(get_base_data_url("invalide"), "must be one of")
})
