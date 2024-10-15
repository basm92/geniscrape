# Helper function to scrape data based on the URL identifier
helper_get_info_from_url <- function(url_identifier, sleep_time = 0.5) {
  real_url <- paste0('https://www.wiewaswie.nl/nl/detail/', url_identifier)
  page <- read_html(real_url)

  # Extract information from the page (similar to `helper_get_info_from_geboorte`)
  left <- page |> html_elements('div.left-column')
  variables_left <- left |> html_elements('div.person dl.dl-horizontal dt') |> html_text2()
  values_left <- left |> html_elements('div.person dl.dl-horizontal dd') |> html_text2()

  right <- page |> html_elements('div.right-column')
  variables_right <- right |> html_elements('dl.dl-horizontal dt') |> html_text2()
  values_right <- right |> html_elements('dl.dl-horizontal dd') |> html_text2()

  # Combine the left and right side data
  left_out <- tibble(var = variables_left, val = values_left)
  right_out <- tibble(var = variables_right, val = values_right)

  # Construct the output tibble
  url_df <- tibble(var = "URL", val = real_url)
  out <- bind_rows(left_out, right_out, url_df)

  Sys.sleep(sleep_time)
  return(out)
}

# Update the `map` function call in `wiewaswie_find_name`
out <- map(url_identifiers, helper_get_info_from_url)
