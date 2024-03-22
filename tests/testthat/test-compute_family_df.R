test_that("compute_family_df works", {
  test_output <- compute_family_df('https://www.geni.com/people/Guillame-le-Blanc-du-Bec-Seigneur-du-Bec/6000000008289847966')
  expect_equal(test_output[1,1]$url, 'https://www.geni.com/people/Hrolf-Bigod/6000000009437210537')
})
