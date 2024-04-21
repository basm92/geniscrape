# https://www.wiewaswie.nl/nl/zoeken/?advancedsearch=1
library(rvest); library(tidyverse); library(httr)

# Start Session
find_cross_section_wiewaswie <- function(place, year, sleep_time=1, type="Geboortes"){
  # Test is a browser session
  test <- read_html_live('https://www.wiewaswie.nl/nl/zoeken/?advancedsearch=1')
  # Start With Place Name
  test$type(css="input[placeholder='Plaats']", text = place)

  # Begin year and End year
  test$type(css="input[ng-model*='PeriodeVan']", text = year)
  test$type(css="input[ng-model*='PeriodeTot']", text = year)

  # Launch Search
  test$click(css="button[ng-click*='DoAdvancedSearch()']")
  Sys.sleep(sleep_time)
  # Filter on Births, Deaths or Marriages
  if(type == "Geboortes"){
    test$click(css="div.search-facets input[ng-value*='DocumentType']")
    test$click(css="li.ng-scope[data-value*='BS Geboorte']")
    Sys.sleep(sleep_time)
    # Filter on the children only
    test$click(css="div.search-facets input[ng-value*='FacetRol']")
    test$click(css="li.ng-scope[data-value*='Kind']")
  }
  if(type == "Huwelijken"){
    test$click(css="div.search-facets input[ng-value*='DocumentType']")
    test$click(css="li.ng-scope[data-value*='BS Huwelijk']")
    Sys.sleep(sleep_time)
    # Filter on the children only
    test$click(css="div.search-facets input[ng-value*='FacetRol']")
    test$click(css="li.ng-scope[data-value*='Bruidegom']")
  }
  if(type == "Overleden"){



  }
  # Loop over pages and over entries within pages
  # How many entries are there on this page?
  url_identifiers <- list()
  while_condition <- TRUE
  Sys.sleep(sleep_time)

  while(while_condition){
    how_many_on_page <- test |>
      html_elements('div.row-toggle.ng-scope') |>
      length()

    for(i in 1:how_many_on_page){
      selector <- paste0('div.row-toggle.ng-scope:nth-of-type(', i, ')', collapse = '')
      # Click an entry
      test$click(css=selector)
      # Extract the URL
      frame <- test |>
        html_elements("iframe[ng-src*='detail'")
      url_identifier <- frame |>
        html_attr('src') |>
        str_extract("\\d+")
      # Add it to the list
      url_identifiers <- c(url_identifiers, url_identifier)
      # Click the entry again to close it
      test$click(css=selector)
      }

    # Try to move to the next page
    while_condition <- length(test$html_elements('a[ng-click*="Page + 1"')) == 1
    if(while_condition){
      test$click(css='a[ng-click*="Page + 1"')
      }
    # Give a little break to prevent flooding
    Sys.sleep(1)
  }

  # Given a list of url identifiers, scrape the info for each of them
  out <- map(url_identifiers, get_info_from_geboorte)

  return(out)
}

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
  out <- bind_rows(left_out, right_out, huwelijk_ouders_out)
  Sys.sleep(sleep_time)
  return(out)
}

testerinho2 <- find_cross_section_wiewaswie("Rijssen", "1817")
# session(url, userAgent = user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36"))

rijssen_1815_1817 <- c(out, testerinho, testerinho2) |>
  map(~ pivot_wider(.x,names_from= var, values_from=val)) |>
  bind_rows()
