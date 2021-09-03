
# https://pkgdown.r-lib.org/reference/build_site.html



########### Document Package ############
.rs.restartR()

# options(rmarkdown.html_vignette.check_title = FALSE)
Sys.setenv('PATH' = paste0('C:/Program Files/qpdf-10.0.1/bin;', Sys.getenv('PATH')))
library(here)
library(devtools)
library(roxygen2)
devtools::document()
setwd("..")
install("NMFSReports")
3
setwd(here::here())
# devtools::check()

########### Create Documentation GitHub-Pages ############

.rs.restartR()
# devtools::install_github("rstudio/fontawesome", force = T)
# library(fontawesome)
library(here)
library(usethis)
library(pkgdown)
# options(rmarkdown.html_vignette.check_title = FALSE)

# git rm -r --cached .

# pkgdown::build_favicons()
devtools::build_vignettes()
usethis::use_pkgdown(config_file = "./pkgdown/_pkgdown.yml")
pkgdown::build_site(pkg = here::here())
# usethis::use_github_action("pkgdown")


# template:
#   params:
#   bootswatch: sandstone

########### Submit to CRAN ############

# devtools::check() # add the console output to cran-commentes.md
# devtools::check_rhub() # check that your email is 'validated.' You'll need a token emailed to you
# devtools::check_win_devel()
# devtools::release()

##########build a vignette#########

# usethis::use_vignette("A_buildReport-test")
# usethis::use_vignette("ScriptLayout")
# usethis::use_vignette("ExampleRunFile")
# usethis::use_vignette("InitialSetUp")

#########NOTES#########

# bootstrap shiny design
# https://gallery.shinyapps.io/117-shinythemes/

# Guide for checking CMD CHECK()
#    https://r-pkgs.org/r-cmd-check.html

# loading qpdf:
#    https://stackoverflow.com/questions/41570633/how-to-build-qpdf-on-windows
#    https://stackoverflow.com/questions/15035150/r-cmd-check-as-cran-warning
#    https://sourceforge.net/projects/qpdf/files/qpdf/10.0.1/
#    Sys.setenv('PATH' = paste0('C:/Program Files/qpdf-10.0.1/bin;', Sys.getenv('PATH')))
#    Sys.which(Sys.getenv("R_QPDF", "qpdf"))

# Compress Files
#    https://stackoverflow.com/questions/10233593/how-to-effectively-deal-with-uncompressed-saves-during-package-check
#    save(land, file="./data/sysdata.rda", compress='xz')

# Add a favicon
#    https://github.com/r-lib/pkgdown/issues/379

# Licence
#   https://stackoverflow.com/questions/56102225/r-devtoolscheck-license-is-not-mentioned-and-other-issues-in-description-file

# Dealing with "no visible binding for global variable '*'"
#   https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when

# clean up gitignore
#   https://stackoverflow.com/questions/48955103/non-standard-file-directory-found-at-top-level-readme-rmd-persists-even-after

# Make code citable
#   https://guides.github.com/activities/citable-code/

# General Reference
#   https://github.com/timjmiller/wham/blob/master/DESCRIPTION
#   date: "`r format(Sys.Date(), format='%B %d %Y') `"

#https://github.com/r-lib/pkgdown/issues/379



######## OLD STUFF #############

#'Replace the first value, if missing, with the next nearest value.
#'
#' If the first value of the timeseries of this column (c) is 0/NaN/NA. Change the first value (and subsequent 0/NaN/NA values) to the first available non-0/NaN/NA value. Then, used in before with 'ReplaceMid'.
#' @param colnames Names of columns to apply this action to.
#' @param temp Name of dataset to apply this action to.
#' @keywords Replace, First
#' @export
#' @examples
#' ReplaceFirst()
ReplaceFirst <- function(colnames, temp) {
  for (c0 in 1:length(colnames)) {
    if (temp[1, colnames[c0]] %in% c(0, NA, NaN, NULL)) {
      findfirstvalue <-
        temp[which(!(temp[, colnames[c0]]  %in% c(0, NA, NaN, NULL))),
             colnames[c0]][1]
      temp[1, colnames[c0]] <- findfirstvalue
    }
  }
  return(temp)
}


#'Replace the first value, if missing, with the next nearest value.
#'
#' If a middle value of the timeseries of this column (c) is 0/NaN/NA. Change the currently 0/NaN/NA value to the previous available non-0/NaN/NA value. Then, used after with 'ReplaceFirst'.
#' @param colnames Names of columns to apply this action to.
#' @param temp Name of dataset to apply this action to.
#' @keywords Replace, Mid, Middle
#' @export
#' @examples
#' ReplaceMid()
ReplaceMid <- function(colnames, temp) {
  for (c0 in 1:length(colnames)) {
    #If a middle value of the timeseries of this column (c) is 0/NaN/NA
    #Change the currently 0/NaN/NA value to the previous available non-0/NaN/NA value
    if (sum(temp[, colnames[c0]] %in% c(0, NA, NaN, NULL)) > 0) {
      troublenumber <- which(temp[, colnames[c0]] %in% c(0, NA, NaN, NULL))
      for (r in 1:length(troublenumber)) {
        findlastvalue <- temp[troublenumber[r] - 1, colnames[c0]][1]
        temp[troublenumber[r], colnames[c0]] <- findlastvalue
      }
    }
  }
  return(temp)
}


