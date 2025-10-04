test_that("get_etalab_arg_pairs returns correct Cartesian product for character vector", {
  res <- get_etalab_arg_pairs(c("72187", "72188"), c("parcelles", "lieux_dits"))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4) # 2 communes * 2 layers
  expect_equal(sort(unique(res$commune)), c("72187", "72188"))
  expect_equal(sort(unique(res$layer)), c("lieux_dits", "parcelles"))
})

test_that("get_etalab_arg_pairs returns correct pairwise data for list", {
  res <- get_etalab_arg_pairs(
    c("72187", "72188"),
    list(
      c("parcelles", "lieux_dits"),
      c("parcelles")
    )
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3) # 2+1 layers
  expect_equal(sort(unique(res$commune)), c("72187", "72188"))
  expect_true(all(c("parcelles", "lieux_dits") %in% res$layer))
})

test_that("get_etalab_arg_pairs throws an error if list length does not match commune length", {
  expect_error(
    get_etalab_arg_pairs(c("72187", "72188"), list(c("parcelles"))),
    "List of data must match commune length"
  )
})

test_that("get_etalab_arg_pairs works with multiple communes and layers (Cartesian product)", {
  communes <- c("72187", "72188")
  layers <- c("parcelles", "lieux_dits")
  res <- get_etalab_arg_pairs(communes, layers)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4) # 2 communes * 2 layers
})

test_that("get_etalab_arg_pairs works with lists for multiple communes", {
  communes <- c("72187", "72188")
  layers_list <- list(c("parcelles", "lieux_dits"), c("parcelles"))
  res <- get_etalab_arg_pairs(communes, layers_list)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3) # 2 + 1 layers
})

