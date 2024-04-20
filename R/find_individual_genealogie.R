find_individual_genealogie <- function(url){
  # Find individual data from genealogieonline.nl entry
  naked_url <- url |> str_extract('.*?(?=/I)')
  geboorte <- gedoopt <- overleden <- naam <- NA
  data <- read_html(url) |>
    html_elements('section#tab-details') 
  
  naam <- data |>
    html_element('h3') |>
    html_text2() |>
    str_remove("Persoonlijke gegevens ") |>
    str_trim()
  
  andere_data <- data |>
    html_elements('ul.nicelist li') |>
    html_text2()
  
  geboorte <- andere_data[str_detect(andere_data, "geboren")]
  gedoopt <-  andere_data[str_detect(andere_data, "gedoopt")]
  overleden <- andere_data[str_detect(andere_data, "overleden")]

  # Ouders naam en link
  ouders_data <- data |>
    html_elements('ul.nicelist li') |>
    html_elements(xpath="//ul[@class='nicelist']//li[contains(text(), 'Een kind van')]//a[contains(@href, '.php')]")
  
  # Ouders namen en links
  onl <- data.frame(names = ouders_data |> html_text2(),
             links = paste0(naked_url, '/', ouders_data |> html_attr('href')))
  
  # Kinderen namen en links
  kinderen_data <- data |>
    html_elements(xpath="//p[contains(text(), 'Kind')]/following-sibling::ol[1]//li")
  
  kinderen_names <- kinderen_data |> html_text2() |> str_remove_all('\\d+|-|>') |> str_trim()
  kinderen_links <- kinderen_data |> html_elements('div') |> html_attr('itemid')
  
  knl <- data.frame(names = kinderen_names,
                    links = kinderen_links)
  
  return(list('naam'=naam,
              'ouders'=onl,
              'geboorte'=geboorte,
              'gedoopt'=gedoopt,
              'overleden'=overleden,
              'kinderen'=knl
              ))
}
