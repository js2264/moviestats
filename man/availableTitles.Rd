% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch.R
\name{availableTitles}
\alias{availableTitles}
\title{A core function}
\usage{
availableTitles(n.pages = 52, verbose = TRUE)
}
\arguments{
\item{n.pages}{an integer. The number of pages to scan}

\item{verbose}{an logical. Should the pages scraping process be verbose?}
}
\value{
a moviesDB object. A list of length 2, the first element of the list is a vector 
  of available titles, the second element contains the corresponding urls.
}
\description{
Used to find all titles available in the first n.pages of The Numbers database
}
\examples{
moviesDB <- availableTitles(n.pages = 10, verbose = TRUE)

}
\seealso{
fetchMovie() searchTitle()
}
\keyword{searchTitle}
