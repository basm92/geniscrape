library(rvest); library(tidyverse); library(httr)

# Main function
wiewaswie_find_name <- function(achternaam = NULL, tussenvoegsel = NULL, voornaam = NULL,
                                patroniem = NULL, beroep = NULL, rol = NULL,
                                periode_start = NULL, periode_end = NULL,
                                land = NULL, place = NULL, sleep_time = 1) {
  # Start session
  name <- read_html_live('https://www.wiewaswie.nl/nl/zoeken/?advancedsearch=1')

  # Fill in the user data
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

  # Launch search
  name$click(css="button[ng-click*='DoAdvancedSearch()']")
  Sys.sleep(sleep_time)

  # Add option later: give users the option to refine (Verfijn resultaten)

  # Loop over pages and entries within pages
  url_identifiers <- list()
  while_condition <- TRUE
  Sys.sleep(sleep_time)

  while(while_condition) {
    # How many results are on the page
    how_many_on_page <- name |>
      html_elements('div.row-toggle.ng-scope') |>
      length()

    # No invalid selectors
    if (how_many_on_page == 0) {
      message("No entries found on this page.")
      break
    }

    for (i in 1:how_many_on_page) {
      Sys.sleep(0.5)
      selector <- paste0('div.row-toggle.ng-scope:nth-of-type(', i, ')')

      # Check that element exists
      if (length(name$html_elements(selector)) > 0) {
        # Click an entry
        name$click(css = selector)
        # Extract the URL
        frame <- name |>
          html_elements("iframe[ng-src*='detail'")

        if (length(frame) > 0) {
          url_identifier <- frame |>
            html_attr('src') |>
            str_extract("\\d+")
      # Add to list
      url_identifiers <- c(url_identifiers, url_identifier)
        }
      # Close entry
      name$click(css=selector)
      } else {
        message("Element not found for index: ", i)
      }
    }

     # Move to the next page if the button exists
    next_button_exists <- length(name$html_elements('a[ng-click*="Page + 1"')) == 1
    if (next_button_exists) {
      name$click(css = 'a[ng-click*="Page + 1"')
    } else {
      while_condition <- FALSE
    }
    Sys.sleep(1)
  }

  # Scrape information based on the type of data (Geborte, Huwelijken, Overlijden, etc.)
  out <- map(url_identifiers, helper_get_info_from_url)

  # Structure the output as a data frame (pivoting to a wide format)
  out <- out |>
    map(~ pivot_wider(.x, names_from = var, values_from = val)) |>
    bind_rows()

  return(out)
}
