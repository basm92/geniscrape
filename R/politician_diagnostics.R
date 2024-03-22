#' Find out whether profile of wife's father or own father is in two prominent lists
#'
#' @param family data.frame with family relations, e.g. father, spouse, mother
#' @param pdc_naam last name of subject in question him/herself
#' @param lijst_ha list higher aangeslagenen; can be NULL
#' @param lijst_elite list elite: can be NULL
#'
#' @return A data.frame with surnames, count siblings and counts on the two lists
#' @export
#'
#' @examples politician_diagnostics(family, "Jansen", lijst_hoogst_aangeslagenen, lijst_elite_moes)
politician_diagnostics <- function(family, pdc_naam, lijst_ha, lijst_elite){
  if(!is.data.frame(family)){
    return(NA)
  }
  # Get URLs and Last Names of Parents
  ## Last Name of Father:
  url_father <- family |>
    dplyr::filter(relation == "Parents1") |>
    dplyr::pull(url)
  if(!rlang::is_empty(url_father)) {
    surname_father <- rvest::read_html(url_father) |>
      rvest::html_elements('p.quiet a') |>
      rvest::html_text2()
    if(rlang::is_empty(surname_father)){
      surname_father <- rvest::read_html(url_father) |>
        rvest::html_elements("h1[itemprop='name']") |>
        rvest::html_text2() |>
        stringr::str_extract("\\w+$")
    }
    surname_father <- surname_father |>
      stringr::str_remove_all("‹ Back to | surname")
  } else{
    surname_father <- NA
  }
  ## Last Name of Mother:
  url_mother <- family |>
    dplyr::filter(relation == "Parents2") |>
    dplyr::pull(url)
  if(!rlang::is_empty(url_mother)) {
    surname_mother <- rvest::read_html(url_mother) |>
      rvest::html_elements('p.quiet a') |>
      rvest::html_text2()
    if(rlang::is_empty(surname_mother)){
      surname_mother <- rvest::read_html(url_mother) |>
        rvest::html_elements("h1[itemprop='name']") |>
        rvest::html_text2() |>
        stringr::str_extract("\\w+$")
    }
    surname_mother <- surname_mother |>
      stringr::str_remove_all("‹ Back to | surname")
  } else{
    surname_mother <- NA
  }
  # Get URLs and Last Names of Wife (Father-in-law) and Mother-in-Law
  url_spouse <- family |>
    dplyr::filter(relation == "Spouse") |>
    dplyr::pull(url)
  if(!rlang::is_empty(url_spouse)){
    surname_spouse <- rvest::read_html(url_spouse) |>
      rvest::html_elements('p.quiet a') |>
      rvest::html_text2()
    if(rlang::is_empty(surname_spouse)){
      surname_spouse <- rvest::read_html(url_spouse) |>
        rvest::html_elements("h1[itemprop='name']") |>
        rvest::html_text2() |>
        stringr::str_extract("\\w+$")
    }
    surname_spouse <- surname_spouse |>
      stringr::str_remove_all("‹ Back to | surname")
  } else {
    surname_spouse <- NA
  }
  # Find out how many siblings
  no_siblings <- family |>
    dplyr::filter(stringr::str_detect(relation, "Sibling")) |>
    nrow()

  # Find out how many of spouse last name and mother last name in Moes LHA and Elite
  count_lha <- lijst_ha |>
    dplyr::filter(last_names != "Jansen", last_names != "Jonge") |>
    dplyr::filter(last_names == surname_spouse |
             last_names == pdc_naam |
             last_names == surname_father |
             last_names == surname_mother) |>
    nrow()

  count_lelite <- lijst_elite |>
    dplyr::filter(last_names != "Jansen", last_names != "Jonge") |>
    dplyr::filter(last_names == surname_spouse |
             last_names == pdc_naam |
             last_names == surname_father |
             last_names == surname_mother) |>
    nrow()

  out <- tibble::tibble(
    surname_father = surname_father,
    surname_mother = surname_mother,
    surname_spouse = surname_spouse,
    no_siblings = no_siblings,
    count_lha = count_lha,
    count_lelite = count_lelite)

  return(out)
}
