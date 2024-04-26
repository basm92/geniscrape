# Compute the ancestors or descendants within n generators
geni_find_ancestors_descendants <- function(url, kind="ancestors", n) {
  # Set filter condition
  filter_condition <- dplyr::if_else(kind == "ancestors" , "Parents", "Children")
  # Helper function to compute ancestors for a single individual
  compute_single_ancestor <- function(url, generation) {
    if (generation == 0) {
      return(NULL)  # Base case: no more generations to compute
    } else {
      # Compute family data frame for the current individual
      family_df <- compute_family_df(url)
      family_df$generation <- n + 1 - generation
      # Filter for parents
      parents_df <- family_df |>
        dplyr::filter(stringr::str_detect(relation, filter_condition))

      # Extract URLs of parents
      parent_urls <- parents_df$url
      # Recursively compute ancestors for each parent and bind the results
      parent_ancestors <- map(parent_urls, ~compute_single_ancestor(., generation - 1)) |>
        list_rbind()

      # Return combined data frame of ancestors for this individual and its parents
      return(bind_rows(parents_df, parent_ancestors))
    }
  }

  # Start recursion with the initial individual
  compute_single_ancestor(url, n)
}

#example
#geni_find_ancestors_descendants('https://www.geni.com/people/Pieter-Philip-van-Bosse/6000000018084773203', n = 2)
