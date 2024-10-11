library(tidyverse); library(rvest)
# Helper: get info from overlijden
helper_get_info_from_overlijden <- function(url_identifier, sleep_time=0.5){
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
  # Put everything together
  left_out <- tibble(var=variables_left,
                     val=values_left)

  right_out <- tibble(var=variables_right,
                      val=values_right)

  url_df <- tibble(var="URL", val=real_url)
  out <- bind_rows(left_out, right_out, url_df)
  Sys.sleep(sleep_time)
  return(out)
}
