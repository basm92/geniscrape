library(rvest); library(tidyverse); library(httr)
source("./R/helpers/wiewaswie_get_info_from_geboorte.R")
source("./R/helpers/wiewaswie_get_info_from_huwelijk.R")
source("./R/helpers/wiewaswie_get_info_from_overlijden.R")

# wiewaswie_extract_info_from_url
# Arg what: c("Geboortes", "Huwelijken", "Overlijden")
wiewaswie_extract_info_from_url <- function(url, what = "Geboortes"){
  url_id <- str_remove(url, "https://www.wiewaswie.nl/nl/detail/")
  out <- NULL
  if(what == "Geboortes"){out <- get_info_from_geboorte(url_id)}
  if(what == "Huwelijken"){out <- get_info_from_huwelijk(url_id)}
  if(what == "Overlijden"){out <- get_info_from_overlijden(url_id)}
  if(length(out) == 0){
    print("Please select one of 'Geboortes', 'Huwelijken', 'Overlijden'")
  }

  return(out)
  }

wiewaswie_extract_info_from_url(test$`Huwelijk kind 1`[3], what="Huwelijken")
