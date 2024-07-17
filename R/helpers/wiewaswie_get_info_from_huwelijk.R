library(tidyverse); library(rvest)

# Helper: get_info_from_huwelijk
get_info_from_huwelijk <- function(url_identifier, sleep_time=0.5){
  real_url <- paste0('https://www.wiewaswie.nl/nl/detail/', url_identifier)
  page <- read_html(real_url)
  # Left side
  left <- page |>
    html_elements('div.left-column')
  variables_left <- left |>
    html_elements('div.person dl.dl-horizontal dt') |>
    html_text2()
  values_left <- left |>
    html_elements('div.person dl.dl-horizontal dd') |>
    html_text2()
  # Right side
  right <- page |>
    html_elements('div.right-column')
  variables_right <- right |>
    html_elements('dl.dl-horizontal dt') |>
    html_text2()
  values_right <- right |>
    html_elements('dl.dl-horizontal dd') |>
    html_text2()
  # Mariage parents
  huwelijk_ouders_bruidegom_url <- page |>
    html_elements('a:contains("Huw. ouders bruidegom")') |>
    html_attr('href')
  huwelijk_ouders_bruid_url <- page |>
    html_elements('a:contains("Huw. ouders bruid")') |>
    html_attr('href')
  huwelijk_ouders_bruid_url <- huwelijk_ouders_bruid_url[!is.element(huwelijk_ouders_bruid_url, huwelijk_ouders_bruidegom_url)]
  # Children
  kinderen <- page |>
    html_elements('a:contains("Geboorte kind")') |>
    html_attr('href')
  # Marriages of children
  huwelijk_kinderen <- page |>
    html_elements('a:contains("Huwelijk dochter"), a:contains("Huwelijk zoon")') |>
    html_attr('href')

  # Put everything together
  left_out <- tibble(var=variables_left,
                     val=values_left)

  right_out <- tibble(var=variables_right,
                      val=values_right)

  huwelijk_ouders_bruidegom_out <- tibble(var="Huwelijk ouders bruidegom URL", val=huwelijk_ouders_bruidegom_url)
  huwelijk_ouders_bruid_out <- tibble(var="Huwelijk ouders bruid URL", val=huwelijk_ouders_bruid_url)
  huwelijk_kinderen_out <- tibble(var="Huwelijk kind", val=huwelijk_kinderen)
  url_df <- tibble(var="URL", val=real_url)
  out <- bind_rows(left_out,
                   right_out,
                   huwelijk_ouders_bruidegom_out,
                   huwelijk_ouders_bruid_out,
                   huwelijk_kinderen_out,
                   url_df)

  out <- out |>
    mutate(var = case_when(
      var == "Beroep" & row_number() == ifelse(length(which(var == "Moeder van de bruidegom")) > 0, (which(var == "Moeder van de bruidegom") + 1), NA) ~ "Beroep Moeder Bruidegom",
      var == "Beroep" & row_number() == ifelse(length(which(var == "Moeder van de bruid")) > 0, (which(var == "Moeder van de bruid") + 1), NA) ~ "Beroep Moeder Bruid",
      var == "Beroep" & row_number() == ifelse(length(which(var == "Vader van de bruidegom")) > 0, (which(var == "Vader van de bruidegom") + 1), NA) ~ "Beroep Vader Bruidegom",
      var == "Beroep" & row_number() == ifelse(length(which(var == "Vader van de bruid")) > 0, (which(var == "Vader van de bruid") + 1), NA) ~ "Beroep Vader Bruid",
      var == "Beroep" & row_number() < ifelse(length(which(var == "Bruid")) > 0, which(var == "Bruid"), NA) ~ "Beroep Bruidegom",
      var == "Geboorteplaats" & row_number() < ifelse(length(which(var == "Bruid")) > 0, which(var == "Bruid"), NA) ~ "Geboorteplaats Bruidegom",
      var == "Leeftijd" & row_number() < ifelse(length(which(var == "Bruid")) > 0, which(var == "Bruid"), NA) ~ "Leeftijd Bruidegom",
      var == "Beroep" & !(row_number() < ifelse(length(which(var == "Bruid")) > 0, which(var == "Bruid"), NA)) ~ "Beroep Bruid",
      var == "Geboorteplaats" & !(row_number() < ifelse(length(which(var == "Bruid")) > 0, which(var == "Bruid"), NA)) ~ "Geboorteplaats Bruid",
      var == "Leeftijd" & !(row_number() < ifelse(length(which(var == "Bruid")) > 0, which(var == "Bruid"), NA)) ~ "Leeftijd Bruid",
      TRUE ~ var
    ))

  out <- out |>
    mutate(var = if_else(var == "Huwelijk kind",
                         paste0(var, " ", row_number() - which(var == "Huwelijk kind")[1] + 1),
                         var))


  Sys.sleep(sleep_time)
  return(out)
}
