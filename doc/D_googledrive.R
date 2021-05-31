## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE, echo = TRUE, warning = FALSE, 
  comment = FALSE, collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(NMFSReports)
library(ggplot2)
library(googledrive)
library(XML)

## ---- eval = FALSE------------------------------------------------------------
#  library("googledrive")
#  
#  drive_deauth()
#  drive_auth()
#  1 # Using the first token you have.
#  
#  googledrive::drive_download("test123123_spreadsheet", # Must be a unique name
#                 type = "csv",
#                 overwrite = TRUE,
#                 path = "./test123123_spreadsheet")
#  
#  a<-readr::read_csv("test123123_spreadsheet.csv")
#  kable(a)

## ---- eval = FALSE------------------------------------------------------------
#  txt <- googledrive_txt_dl(filename_gd = "test123123_doc",
#                            filename_dl = "test123123_doc_downloaded",
#                            path = "./",
#                            verbose = FALSE)
#  txt

