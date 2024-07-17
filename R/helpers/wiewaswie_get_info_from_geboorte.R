library(rvest); library(tidyverse)
# Helper: get_info_from_geboorte
get_info_from_geboorte <- function(url_identifier, sleep_time=0.5){
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
  huwelijk_ouders_url <- page |>
    html_elements('a:contains("Huwelijk ouders")') |>
    html_attr('href')
  # Put everything together
  left_out <- tibble(var=variables_left,
                     val=values_left)

  right_out <- tibble(var=variables_right,
                      val=values_right)

  huwelijk_ouders_out <- tibble(var="Huwelijk ouders URL", val=huwelijk_ouders_url)
  url_df <- tibble(var="URL", val=real_url)
  out <- bind_rows(left_out, right_out, huwelijk_ouders_out, url_df)
  # Mutate the Profession if there
  out <- out |>
    mutate(var = case_when(var == "Beroep" & row_number() < ifelse(length(which(var == "Moeder")) > 0, which(var == "Moeder"), NA)~ "Beroep Vader",
                           var == "Beroep" & row_number() > ifelse(length(which(var == "Moeder")) > 0, which(var == "Moeder"), NA)~ "Beroep Moeder",
                           TRUE ~ var))
  Sys.sleep(sleep_time)
  return(out)
}

