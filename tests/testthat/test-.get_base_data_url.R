library(testthat)
library(rcadastre)

test_that("get_base_data_url returns correct URLs", {
  # Test pour 'pci'
  expect_equal(
    get_base_data_url("pci"),
    "https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur"
  )

  # Test pour 'etalab'
  expect_equal(
    get_base_data_url("etalab"),
    "https://cadastre.data.gouv.fr/data/etalab-cadastre"
  )

  # Test que la valeur de retour est un caractère
  expect_type(get_base_data_url("pci"), "character")
})

test_that("get_base_data_url handles invalid input", {
  # Test que la fonction renvoie une erreur pour un argument invalide
  expect_error(get_base_data_url("invalide"), "must be one of")
})
