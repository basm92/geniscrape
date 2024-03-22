#' Helper to parse dates from Geni.com
#'
#' @param family data.frame with relations from geni.com
#'
#' @return data.frame with parsed dates
#' @export
#'
#' @examples parse_dates(family)
parse_dates <- function(family){
  if(!is.data.frame(family)){
    return(NA)
  }
  df <- family |>
    dplyr::rowwise() |>
    dplyr::mutate(birthdate = dplyr::case_when(
      !is.na(readr::parse_date(birthdate, format = "%B %d, %Y")) ~ readr::parse_date(birthdate, format =  "%B %d, %Y"),
      stringr::str_detect(birthdate, "^\\d{4}$") ~ lubridate::dmy(paste0("01-01-", stringr::str_extract(birthdate, "^\\d{4}$"), collapse='')),
      stringr::str_detect(birthdate, "\\d{4}") ~ lubridate::dmy(paste0("01-01-", stringr::str_extract(birthdate, "\\d{4}"), collapse='')),
      TRUE ~  lubridate::dmy(paste0("01-01-", str_extract(birthdate, "\\d{4}"), collapse=''))
    ))

  df <- df|>
    dplyr::mutate(deathdate = dplyr::case_when(
      !is.na(readr::parse_date(deathdate, format = "%B %d, %Y")) ~ readr::parse_date(deathdate, format =  "%B %d, %Y"),
      stringr::str_detect(deathdate, "^\\d{4}$") ~ lubridate::dmy(paste0("01-01-", stringr::str_extract(deathdate, "^\\d{4}$"), collapse='')),
      stringr::str_detect(deathdate, "\\d{4}") ~ lubridate::dmy(paste0("01-01-", stringr::str_extract(deathdate, "\\d{4}"), collapse='')),
      TRUE ~  lubridate::dmy(paste0("01-01-", str_extract(deathdate, "\\d{4}"), collapse=''))
    ))
  df <- df |>
    dplyr::ungroup()

  return(df)
}
