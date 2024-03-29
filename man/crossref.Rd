% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NMFSReports.R
\name{crossref}
\alias{crossref}
\title{Reference a figure, table, or equation with an anchored tag}
\usage{
crossref(list_obj, nickname, sublist = "number", exact = TRUE, text = TRUE)
}
\arguments{
\item{list_obj}{A list object created by list_figures or list_tables.}

\item{nickname}{A unique string that is used to identify the plot or table in list_figures or list_tables, respectively.}

\item{sublist}{A string of the sublist in list_figures or list_tables you want the contents returned from.}

\item{exact}{T/F. If TRUE, 'nickname' must match the name of the list item exactly. If FALSE, crossref will return all entries with that string fragment. Default = TRUE.}

\item{text}{T/F. If TRUE, will output results prepared for a text output. If FALSE, will output each element. Default = TRUE.}
}
\value{
The item in the list.
}
\description{
Reference a figure, table, or equation with an anchored tag
}
\examples{
list_figures <- c()
table_raw <- data.frame(x = 1, y = 1)
pp <- plot(x = table_raw$x, y = table_raw$y)
list_figures <- NMFSReports::save_figures(
   figure = pp,
   list_figures = list_figures,
   header = "blah blah blah",
   nickname = "example_1", # a unique name you can refer back to
   cnt_chapt_content = "003",
   cnt = "012")
list_figures <- NMFSReports::save_figures(
   figure = pp,
   list_figures = list_figures,
   header = "blah blah blah",
   nickname = "example2", # a unique name you can refer back to
   cnt_chapt_content = "003",
   cnt = "013")
list_figures
refnum <- crossref(
   list_obj = list_figures,
   nickname = "example_1",
   sublist = "number")
refnum
print(paste0("Please refer to figure ", refnum,
             " to see this figure, not the other figure."))
# example using a partial phrase with `exact = FALSE`
crossref(
   list_obj = list_figures,
   nickname = "example_",
   sublist = "number",
   exact = FALSE)
# using a wildard with `exact = FALSE`
crossref(
 list_obj = list_figures,
 nickname = "example*1",
 sublist = "number",
 exact = FALSE)
crossref(
 list_obj = list_figures,
 nickname = "example*",
 sublist = "number",
 exact = FALSE,
 text = FALSE)
refnum <- crossref(
 list_obj = list_figures,
 nickname = "example*",
 sublist = "number",
 exact = FALSE,
 text = TRUE)
refnum
print(paste0("Please refer to figure ", refnum,
             " to see this figure, not the other figure."))
}
