library(rvest); library(tidyverse); library(httr)

# Main function
wiewaswie_find_name <- function(achternaam = NULL, tussenvoegsel = NULL, voornaam = NULL,
                                patroniem = NULL, beroep = NULL, rol = NULL,
                                periode_start = NULL, periode_end = NULL,
                                land = NULL, place = NULL, type = NULL,
                                role_filter = NULL, sleep_time = 1) {
  # Go to wiewaswie advanced search
  name <- read_html_live('https://www.wiewaswie.nl/nl/zoeken/?advancedsearch=1')

  # User can fill in data
  if (!is.null(achternaam)) name$type(css="input[placeholder='Achternaam']", text = achternaam)
  if (!is.null(tussenvoegsel)) name$type(css="input[placeholder='Tussenvoegsel']", text = tussenvoegsel)
  if (!is.null(voornaam)) name$type(css="input[placeholder='Voornaam']", text = voornaam)
  if (!is.null(patroniem)) name$type(css="input[placeholder='Patroniem']", text = patroniem)
  if (!is.null(beroep)) name$type(css="input[placeholder='Beroep']", text = beroep)
  if (!is.null(rol)) name$type(css="input[placeholder='Rol']", text = rol)
  if (!is.null(periode_start)) name$type(css="input[ng-model*='PeriodeVan']", text = year)
  if (!is.null(periode_end)) name$type(css="input[ng-model*='PeriodeTot']", text = year)
  if (!is.null(land)) name$type(css="input[placeholder='Land']", text = land)
  if (!is.null(place)) name$type(css="input[placeholder='Plaats']", text = place)

  # Start search
  name$click(css="button[ng-click*='DoAdvancedSearch()']")
  Sys.sleep(sleep_time)

  # Apply filters if specified
  if (!is.null(type)) {
    name$click(css="div.search-facets input[ng-value*='DocumentType']")

    if (type == "Geboortes") {
      name$click(css="li.ng-scope[data-value*='BS Geboorte']")
    } else if (type == "Huwelijken") {
      name$click(css="li.ng-scope[data-value*='BS Huwelijk']")
    } else if (type == "Overlijden") {
      name$click(css="li.ng-scope[data-value*='BS Overlijden']")
    }
    Sys.sleep(sleep_time)
  }

  if (!is.null(role_filter)) {
    name$click(css="div.search-facets input[ng-value*='FacetRol']")

    if (role_filter == "Kind") {
      name$click(css="li.ng-scope[data-value*='Kind']")
    } else if (role_filter == "Bruidegom") {
      name$click(css="li.ng-scope[data-value*='Bruidegom']")
    } else if (role_filter == "Overledene") {
      name$click(css="li.ng-scope[data-value*='Overledene']")
    }
    Sys.sleep(sleep_time)
  }

  # Loop over pages and entries within pages
  url_identifiers <- list()
  while_condition <- TRUE
  Sys.sleep(sleep_time)

  while(while_condition) {
    # Get the number of results on the page
    how_many_on_page <- name |>
      html_elements('div.row-toggle.ng-scope') |>
      length()

    if (how_many_on_page == 0) {
      message("No entries found on this page.")
      break
    }

    # Iterate through each entry on the page
    for (i in 1:how_many_on_page) {
      Sys.sleep(0.5)
      selector <- paste0('div.row-toggle.ng-scope:nth-of-type(', i, ')')

      # Check if the entry exists before clicking
      if (length(name$html_elements(selector)) > 0) {
        # Click on the entry to open it
        name$click(css = selector)
        Sys.sleep(0.5)  # Ensure page is loaded

        # Extract the URL
        frame <- name |>
          html_elements("iframe[ng-src*='detail']")

        if (length(frame) > 0) {
          url_identifier <- frame |>
            html_attr('src') |>
            str_extract("\\d+")

          # Add to the list of URLs
          url_identifiers <- c(url_identifiers, url_identifier)
        }

        # Close the entry
        name$click(css = selector)
      } else {
        message("Element not found for index: ", i)
      }
    }

    # Check if the next page button exists and click it if present
    next_button <- name |>
      html_elements('a[ng-click*="Page + 1"]')

    if (length(next_button) > 0) {
      name$click(css = 'a[ng-click*="Page + 1"]')
      Sys.sleep(1)  # Allow time for the new page to load
    } else {
      while_condition <- FALSE
    }
  }

  # Scrape the information from each URL
  out <- map(url_identifiers, helper_get_info_from_url)

  # Structure the output as a data frame (pivot to wide format)
  out <- out |>
    map(~ pivot_wider(.x, names_from = var, values_from = val)) |>
    bind_rows()

  return(out)
}
