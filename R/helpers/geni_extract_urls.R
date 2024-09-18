#' Helper to Extract URLS of Family Members given a Geni.com URL
#'
#' @param link URL to a Geni.com page
#'
#' @return URLs of Parents, Children, Siblings and Spouse(s)
#'
#' @examples extract_urls('https://www.geni.com/people/Guillame-le-Blanc-du-Bec-Seigneur-du-Bec/6000000008289847966')
extract_urls <- function(link){
  # Read HTML Code
  html <- rvest::read_html(link)

  # Find where the names are
  data_structure <- html |>
    rvest::html_elements(xpath="//*[@id='family_handprint']") |>
    rvest::html_text() |>
    stringr::str_split("\n") |>
    purrr::map(stringr::str_trim) |>
    purrr::pluck(1) |>
    purrr::keep(~ stringr::str_detect(.x, "Son of|Brother of|Father of|Husband of| Wife of|Daughter of|Sister of|Mother of"))

  # Extract the names without the noise
  parent_names <- data_structure[which(stringr::str_detect(data_structure, "Son of|Daughter of"))] |>
    stringr::str_remove_all("Son of |Daughter of | « less|\\d others") |>
    stringr::str_split(" and ") |>
    purrr::map(~ stringr::str_remove_all(.x, " '.*|'.*")) |>
    purrr::map(stringr::str_trim) |>
    purrr::flatten() |>
    purrr::discard(~ .x == "")
  sibling_names <- data_structure[which(stringr::str_detect(data_structure, "Brother of|Sister of"))] |>
    stringr::str_remove_all("Brother of |Sister of | « less|\\d others") |>
    stringr::str_split(" and |;") |>
    purrr::map(~ stringr::str_remove_all(.x, " '.*|'.*")) |>
    purrr::map(stringr::str_trim) |>
    purrr::flatten() |>
    purrr::discard(~ .x == "")
  children_names <-  data_structure[which(stringr::str_detect(data_structure, "Father of|Mother of"))] |>
    stringr::str_remove_all("Father of |Mother of | « less|\\d others") |>
    stringr::str_split(" and |;") |>
    purrr::map(~ stringr::str_remove_all(.x, " '.*|'.*")) |>
    purrr::map(stringr::str_trim) |>
    purrr::flatten() |>
    purrr::discard(~ .x == "")
  spouse_names <- data_structure[which(stringr::str_detect(data_structure, "Husband of|Wife of"))] |>
    stringr::str_remove_all("Husband of |Wife of | « less|\\d others") |>
    stringr::str_split(" and |;") |>
    purrr::map(~ stringr::str_remove_all(.x, " '.*|'.*")) |>
    purrr::map(stringr::str_trim) |>
    purrr::flatten() |>
    purrr::discard(~ .x == "")

  # Parent URLS
  parent_urls <- parent_names |>
    purrr::map(~ {
      xpath_selector <- paste0(
        "//text()[contains(., 'Son of') or contains(., 'Daughter of')]/following::a[contains(text(), '",
        .x,
        "') and starts-with(@href, 'https://www.geni.com')][1]",
        collapse='')

      out <- html |>
        rvest::html_elements(xpath=xpath_selector) |>
        rvest::html_attr('href')
      return(out)
    })

  # Children URLS
  children_urls <- children_names |>
    purrr::map(~ {
      xpath_selector <- paste0(
        "//text()[contains(., 'Father of') or contains(., 'Mother of')]/following::a[contains(text(), '",
        .x,
        "') and starts-with(@href, 'https://www.geni.com')][1]",
        collapse='')

      out <- html |>
        rvest::html_element(xpath=xpath_selector) |>
        rvest::html_attr('href')

      return(out)
    })

  # Siblings URLS
  sibling_urls <- sibling_names |>
    purrr::map(~ {
      xpath_selector <- paste0(
        "//text()[contains(., 'Brother of') or contains(., 'Sister of')]/following::a[contains(text(), '",
        .x,
        "') and starts-with(@href, 'https://www.geni.com')][1]",
        collapse='')

      out <- html |>
        rvest::html_elements(xpath=xpath_selector) |>
        rvest::html_attr('href')

      return(out)
    })

  # Spouse URLS
  spouse_urls <- spouse_names |>
    purrr::map(~ {
      xpath_selector <- paste0(
        "//text()[contains(., 'Husband of') or contains(., 'Wife of')]/following::a[contains(text(), '",
        .x,
        "') and starts-with(@href, 'https://www.geni.com')][1]",
        collapse='')

      out <- html |>
        rvest::html_elements(xpath=xpath_selector) |>
        rvest::html_attr('href')

      return(out)
    })

  # Output them in a list
  return(list("Parents"=parent_urls,
              "Children"=children_urls,
              "Siblings"=sibling_urls,
              "Spouse"=spouse_urls))

}
