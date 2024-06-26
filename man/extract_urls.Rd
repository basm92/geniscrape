% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_urls.R
\name{extract_urls}
\alias{extract_urls}
\title{Helper to Extract URLS of Family Members given a Geni.com URL}
\usage{
extract_urls(link)
}
\arguments{
\item{link}{URL to a Geni.com page}
}
\value{
URLs of Parents, Children, Siblings and Spouse(s)
}
\description{
Helper to Extract URLS of Family Members given a Geni.com URL
}
\examples{
extract_urls('https://www.geni.com/people/Guillame-le-Blanc-du-Bec-Seigneur-du-Bec/6000000008289847966')
}
