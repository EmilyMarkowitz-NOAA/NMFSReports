## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE, echo = TRUE, warning = FALSE, 
  comment = FALSE, collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(NMFSReports)

## -----------------------------------------------------------------------------
R.version

## -----------------------------------------------------------------------------
str(allPackage <- installed.packages(.Library, priority = "high"))

allPackage [, c(1,3:5)]

