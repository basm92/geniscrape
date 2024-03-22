#' Scrape Moes Data
#'
#' @return A list of df's with the Moes data
#'
#' @export
#' @examples dfs <- scrape_moes_data()
scrape_moes_data <- function(){
  doc <- pdftools::pdf_text('https://pure.rug.nl/ws/portalfiles/portal/14087329/Bijlage_II.pdf')
  doc <- doc[9:1384] # Relevant part
  lha <- stringr::str_extract_all(doc, "•(.*?\n.*?\n)") |>
    purrr::flatten()

  last_names <- lha |>
    stringr::str_extract_all("^[^,]+") |>
    stringr::str_remove_all("• ")

  first_names_and_prefixes <- lha |>
    stringr::str_extract_all(",(.*?)\\(")

  prefixes <- first_names_and_prefixes |>
    stringr::str_extract_all("\\b[a-z\\.][\\w\\.]*\\b") |>
    purrr::map(~ paste0(.x, collapse=' ')) |>
    stringr::str_remove_all('mr|dr') |>
    stringr::str_remove_all("\\.[A-Z]") |>
    stringr::str_squish()

  titles <- first_names_and_prefixes |>
    stringr::str_extract_all(" dr| jhr| mr| Jhr| Dr| Mr") |>
    purrr::map(~ paste0(.x, collapse='')) |>
    stringr::str_squish()

  first_names <- first_names_and_prefixes |>
    stringr::str_remove_all("\\b[a-z]\\w*\\b|Jhr|\\(|,|\\. |'") |>
    stringr::str_squish()

  dob <- lha |>
    stringr::str_extract_all("geboren\\s+op\\s+(\\S+\\s+\\S+\\s+\\S+)") |>
    stringr::str_remove_all("geboren op ")

  pob <- lha |>
    stringr::str_extract_all("(?<=,)(.*)") |>
    stringr::str_extract_all("te\\s+(\\S+,)") |>
    stringr::str_remove_all("te |,|te\n") |>
    stringr::str_squish()

  place_of_living <- lha |>
    stringr::str_extract("(?<=wonende)([\\s\\S]*)") |>
    stringr::str_remove_all("Grondbelasting|Personele belasting|Patentbelasting|Totaal") |>
    stringr::str_remove_all("\n") |>
    stringr::str_remove_all(" te ") |>
    stringr::str_remove_all("\\)") |>
    stringr::str_squish()

  # Construct a data.frame
  df <- tibble::tibble(first_names = first_names,
                       last_names = last_names,
                       prefixes = prefixes,
                       dob = dob,
                       pob = pob,
                       place_of_living = place_of_living)

  #write_csv2(df, "./data/wealth/lijst_hoogst_aangeslagenen.csv")

  # Now scrape the other elite list
  doc <- pdftools::pdf_text('https://pure.rug.nl/ws/portalfiles/portal/14087328/Bijlage_I.pdf')
  doc <- doc[46:49]

  raw_names <- doc |>
    stringr::str_split("\\)") |>
    purrr::flatten() |>
    stringr::str_trim() |>
    stringr::str_split("\n") |>
    purrr::flatten() |>
    stringr::str_trim()

  cleaner_names <- raw_names |>
    purrr::discard(~ stringr::str_detect(.x, "ONDER ARISTOCRATEN|40[0-5]")) |>
    purrr::discard(~ .x == "")

  intensity <- cleaner_names |>
    stringr::str_extract("[0-9]+") |>
    as.numeric()

  last_names_with_prefixes <- cleaner_names |>
    stringr::str_remove_all("\\([0-9]+|≥[0-9]|\\(") |>
    stringr::str_trim()

  prefixes <- last_names_with_prefixes |>
    stringr::str_extract(",(.*)+") |>
    stringr::str_remove(",") |>
    stringr::str_trim()

  last_names <- last_names_with_prefixes |>
    stringr::str_extract("[^,]*")

  df2 <- tibble::tibble(last_names=last_names,
                        prefixes = prefixes,
                        intensity = intensity) |>
    dplyr::filter(last_names != "JAAP MOES")

  #write_csv2(df2, "./data/wealth/lijst_elite_moes.csv")
  return(list(df, df2))
}
