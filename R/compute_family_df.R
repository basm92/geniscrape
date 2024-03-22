#' Compute a data.frame of family relations for a start URL
#'
#' @param start_url URL in a Geni.com page
#'
#' @return A data.frame with family relations
#' @export
#'
#' @examples compute_family_df('https://www.geni.com/people/Guillame-le-Blanc-du-Bec-Seigneur-du-Bec/6000000008289847966')
compute_family_df <- function(start_url){
  if(!is.na(start_url)){
    # Extract the family members
    family_members <- extract_urls(start_url)
    # Filter the NA and clean
    family_members_clean <- purrr::map(family_members, purrr::discard, ~ length(.x) < 1)
    family_members_clean <- purrr::map(family_members_clean, purrr::discard, is.na)

    # Flatten for simplicity
    fm_flat <- family_members_clean |> purrr::flatten()

    data <- purrr::map(fm_flat, get_info_from_url) |>
      dplyr::bind_rows()

    # Extract the Relation Types
    combined_data <- data |>
      dplyr::mutate(relation = names(unlist(family_members_clean)))

    if(ncol(combined_data) > 0){
      combined_data <- combined_data |>
        dplyr::distinct(url, .keep_all=T)
    }-
      return(combined_data)

  } else{
    return(NA)
  }
}
