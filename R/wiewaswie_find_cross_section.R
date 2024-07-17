# https://www.wiewaswie.nl/nl/zoeken/?advancedsearch=1
library(rvest); library(tidyverse); library(httr)
source("./R/helpers/wiewaswie_get_info_from_geboorte.R")
source("./R/helpers/wiewaswie_get_info_from_huwelijk.R")
source("./R/helpers/wiewaswie_get_info_from_overlijden.R")

# Start Session
wiewaswie_find_cross_section <- function(place, year, sleep_time=1, type="Geboortes"){
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
  if(type == "Overlijden"){
    test$click(css="div.search-facets input[ng-value*='DocumentType']")
    test$click(css="li.ng-scope[data-value*='BS Overlijden']")
    Sys.sleep(sleep_time)
    # Filter on the children only
    test$click(css="div.search-facets input[ng-value*='FacetRol']")
    test$click(css="li.ng-scope[data-value*='Overledene']")
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
      Sys.sleep(0.5)
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
  if(type=="Geboortes"){out <- map(url_identifiers, get_info_from_geboorte)}
  if(type=="Huwelijken"){out <- map(url_identifiers, get_info_from_huwelijk)}
  if(type=="Overlijden"){out <- map(url_identifiers, get_info_from_overlijden)}

  # Pivot the stuff to a data.frame
  out <- out |>
    map(~ pivot_wider(.x, names_from=var, values_from = val)) |>
    bind_rows()
  return(out)

}

# example:
test <- find_cross_section_wiewaswie("Hellendoorn", "1850", type="Overlijden")
