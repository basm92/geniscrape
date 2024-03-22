test_that("scrape_moes_data works", {
  dfs <- scrape_moes_data()
  expect_equal(dfs[[1]][5,5]$pob, "'s-Hertogenbosch")
})
