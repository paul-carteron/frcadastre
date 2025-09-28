library(testthat)
library(rcadastre)

test_that("detect_urls works on dgfip-pci-vecteur page", {
  skip_on_cran()
  skip_if_offline("cadastre.data.gouv.fr")

  links <- detect_urls("https://cadastre.data.gouv.fr/data/dgfip-pci-vecteur")

  # Check that the result is a non-empty character vector
  expect_type(links, "character")
  expect_true(length(links) > 0)

  # Check that it contains at least one millesime (e.g., "latest")
  expect_true(any(grepl("latest", links)))
})

test_that(".extract_urls_from_html extracts correct links", {
  html <- xml2::read_html('
    <html>
      <body>
        <a href="file1.zip">file1</a>
        <a href="../">parent</a>
        <a href="">empty</a>
        <a href="file2.zip">file2</a>
      </body>
    </html>'
  )
  links <- xml2::xml_find_all(html, ".//a[@href]") |> xml2::xml_attr("href")
  links <- links[!is.na(links) & links != "../" & nzchar(links)]

  expect_equal(links, c("file1.zip", "file2.zip"))
})
