
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geniscrape

<!-- badges: start -->
<!-- badges: end -->

The goal of geniscrape is to scrape genealogies and lineages from
Geni.com in an efficient an easy way.

## Installation

You can install the development version of geniscrape like so:

``` r
library(devtools)
install_github("basm92/geniscrape")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(geniscrape)
compute_family_df("https://www.geni.com/people/Lucas-Brouwer/6000000037332972722")
#> # A tibble: 3 × 6
#>   url                                   birthdate deathdate pob   pod   relation
#>   <chr>                                 <chr>     <chr>     <chr> <chr> <chr>   
#> 1 https://www.geni.com/people/Maria-Br… Septembe… October … Alme… ""    Childre…
#> 2 https://www.geni.com/people/Barend-B… June 14,… February… Alme… "Arn… Childre…
#> 3 https://www.geni.com/people/Grietje-… August 0… March 07… Vrie… "Alm… Spouse
```

There are also two pre-loaded datasets by Moes (2012) included:

``` r
lijst_elite_moes
#> # A tibble: 272 × 3
#>    last_names              prefixes intensity
#>    <chr>                   <chr>        <dbl>
#>  1 Ablaing van Giessenburg d'               1
#>  2 Büchner                 <NA>             1
#>  3 Akerlaken               Van              1
#>  4 Bultman                 <NA>             1
#>  5 Alberda van Ekenstein   <NA>             4
#>  6 Bylandt                 Van              5
#>  7 Alphen                  Van              1
#>  8 Carsten                 <NA>             2
#>  9 Andringa de Kempenaer   Van              5
#> 10 Casembroot              De               2
#> # ℹ 262 more rows

lijst_hoogst_aangeslagenen
#> # A tibble: 4,738 × 6
#>    first_names    last_names              prefixes dob     pob   place_of_living
#>    <chr>          <chr>                   <chr>    <chr>   <chr> <chr>          
#>  1 Johannes Eliza Aa Criellaert           van der  18 nov… Rott… Rotterdam      
#>  2 Josua          Aa Criellaert           van der  8 apri… Rott… Rotterdam      
#>  3 Cornelis       Aarden                  <NA>     26 feb… Wouw  Standaardbuiten
#>  4 Gijsbert       Aardenne                van      10 maa… Dord… Dordrecht      
#>  5 Antonie        Abbema                  <NA>     18 sep… 's-H… 's-Hertogenbos…
#>  6 Abel           Abbring                 <NA>     7 augu… Beem… Rasquert (Baflo
#>  7 J.D.C.C.W      Ablaing van Giessenburg baron d  4 janu… Utre… Doorn          
#>  8 Willem Jan     Ablaing van Giessenburg baron d  1 juli… Amst… 's-Gravenhage  
#>  9 Taede Ruurds   Abma                    <NA>     30 jan… Fols… Folsgare       
#> 10 Geerd Hendriks Addens                  <NA>     2 okto… Bell… Bellingewolde  
#> # ℹ 4,728 more rows
```
