library(httr); library(rvest); library(tidyverse)

test <- httr::GET('https://www.genealogieonline.nl/zoeken/index.php?pn=rijssen&gv=1801&gt=1805')
test <- read_html("https://www.genealogieonline.nl/zoeken/index.php?pn=Rijssen")
rvest::read_html_live('https://www.genealogieonline.nl/zoeken/index.php?pn=rijssen&gv=1801&gt=1805')

find_cross_section_genealogie <- function(place, begin_year, end_year){
  # Given a place and a timespan (can be 1 year, begin_year = end_year)
  # Return unique URLS to individuals
  base_url <- 'https://www.genealogieonline.nl/zoeken/index.php?'
  place_name <- paste0('pn=', place)
  begin_yr <- paste0('gv=', begin_year)
  end_yr <- paste0('gt=', end_year)

  start_url <- paste(base_url, place_name, begin_yr, end_yr, '&ta=100', sep='&')
  ses <- session(start_url)

  while_condition <- TRUE
  url <- start_url
  while (while_condition) {
    # Code to execute within the loop
      ses <- session_follow_link(ses, xpath="//a[text()='Volgende']")
      url <- c(url, ses$url)
      if(url[length(url)] == url[length(url)-1]){
      # Change the condition to exit the loop
      while_condition <- FALSE
      url <- url[1:length(url)-1]
      print("Finished.")
      }

  names_and_links <- map(url, extract_from_url)

  }
  # Clean the names and links
  names <- names_and_links |> map(~ .x |> pluck(1)) |> reduce(c)
  links <- names_and_links |> map(~ .x |> pluck(2)) |> reduce(c)
  return(tibble(names=names, links=links))
}

# Helper
extract_from_url <- function(url){

  links_and_data <- read_html_live(url) |>
    html_elements('div.col-md-8 ul.nicelist li') |>
    html_element('a')

  names <- links_and_data |>
    html_text2()

  links <- links_and_data |>
    html_attr('href')

  return(list("names"=names, "links"=links))

}
