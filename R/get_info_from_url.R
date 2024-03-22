#' Helper to get the information of all relatives
#'
#' @param url A geni.com URL
#'
#' @return data.frame with url, birth date, death date, pob, pod
#' @export
#'
#' @examples get_info_from_url('https://www.geni.com/people/Lucas-Brouwer/6000000037332972722')
get_info_from_url <- function(url){

  html <- rvest::read_html(url)
  birthdate <- html |>
    rvest::html_elements("time#birth_date") |>
    rvest::html_text()
  deathdate <- html |>
    rvest::html_elements("span[itemprop='deathDate']") |>
    rvest::html_text() |>
    stringr::str_trim()
  pob <- html |>
    rvest::html_elements("td#birth_location span[itemprop='name']") |>
    rvest::html_text() |>
    stringr::str_trim()
  pod <- html |>
    rvest::html_elements(xpath="//span[@itemprop='deathDate']/following::text()[2]") |>
    rvest::html_text() |>
    stringr::str_trim()

  together <- list(url = url,
                   birthdate = birthdate,
                   deathdate = deathdate,
                   pob = pob,
                   pod = pod)

  together <- together |>
    purrr::map(~ if (length(.x) < 1) NA_character_ else .x)

  out <- together |>
    tibble::as_tibble()

  return(out)
}
