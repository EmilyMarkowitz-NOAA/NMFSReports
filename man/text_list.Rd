% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NMFSReports.R
\name{text_list}
\alias{text_list}
\title{Takes a string of words and combines them into a sentance that lists them.}
\usage{
text_list(x, oxford = TRUE)
}
\arguments{
\item{x}{Character strings you want in your string.}

\item{oxford}{T/F: would you like to use an oxford comma? Default = TRUE}
}
\description{
This function alows you to take a string of words and combine them into a sentance list. For example, 'apples', 'oranges', 'pears' would become 'apples, oranges, and pears'. This function uses oxford commas.
}
\examples{
text_list(c(1,2,"hello",4,"world",6))
}
\keyword{strings}
