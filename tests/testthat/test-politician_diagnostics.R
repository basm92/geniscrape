test_that("politician_diagnostics works", {
  df <- compute_family_df("https://www.geni.com/people/Lucas-Brouwer/6000000037332972722")
  test_output <- politician_diagnostics(df,
                                        "Brouwer",
                                        geniscrape::lijst_hoogst_aangeslagenen,
                                        geniscrape::lijst_elite_moes)
  expect_equal(test_output[1,3]$surname_spouse, "Kruijs")
})
