# Compute the ancestors or descendants within n generators

geni_find_ancestors_descendants <- function(url, kind="ancestors", n, combined_relatives=list()){
  # Initialize
  i <- 1
  filter_condition <- dplyr::if_else(kind == "ancestors" , "Parents", "Children")

  while(i <= n){
    if(i == 1){
    relatives <- compute_family_df(url) |>
      dplyr::filter(stringr::str_detect(relation, filter_condition)) |>
      list()
    } else {
      relatives <- relatives |>
        map(~ .x |>
              dplyr::filter(stringr::str_detect(relation, filter_condition)))

      urls_list <- purrr::map(relatives, ~ .x |>
                                pull(url))
      urls_list |>
        map(~ compute_family_df(.x) |>
              dplyr::filter(stringr::str_detect(relation, filter_condition)))
    }
    combined_relatives <- c(combined_relatives, relatives)
    i <- i+1
  }

  return(combined_relatives)
}

geni_find_ancestors_descendants('https://www.geni.com/people/Pieter-Philip-van-Bosse/6000000018084773203', n = 2)
