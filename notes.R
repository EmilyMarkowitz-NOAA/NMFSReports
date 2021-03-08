
# https://pkgdown.r-lib.org/reference/build_site.html

########### Document Package ############
.rs.restartR()

options(rmarkdown.html_vignette.check_title = FALSE)
Sys.setenv('PATH' = paste0('C:/Program Files/qpdf-10.0.1/bin;', Sys.getenv('PATH')))
library(here)
library(devtools)
library(roxygen2)
devtools::document()
setwd("..")
install("NMFSReports")
3
setwd(here())
# devtools::check()

########### Create Documentation GitHub-Pages ############

.rs.restartR()
# devtools::install_github("rstudio/fontawesome", force = T)
# library(fontawesome)
library(here)
library(usethis)
library(pkgdown)
options(rmarkdown.html_vignette.check_title = FALSE)


# pkgdown::build_favicons()
devtools::build_vignettes()
usethis::use_pkgdown(config_file = "./pkgdown/_pkgdown.yml")
pkgdown::build_site(pkg = here())
usethis::use_github_action("pkgdown")

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



priceIndex <- function (x, pvar, qvar, pervar, indexMethod = "laspeyres",
                        prodID, sample = "matched", output = "pop", chainMethod = "pop",
                        sigma = 1.0001, ...)
{
  validMethods <- c("dutot", "carli", "jevons",
                    "harmonic", "cswd", "laspeyres", "paasche",
                    "fisher", "tornqvist", "satovartia",
                    "walsh", "ces", "geomlaspeyres", "geompaasche")
  if (!(tolower(indexMethod) %in% validMethods)) {
    stop("Not a valid index number method.")
  }
  validOutput <- c("chained", "pop", "fixedbase")
  if (!(tolower(output) %in% validOutput)) {
    stop("Not a valid output type. Please choose from chained, fixedbase or pop.")
  }
  colNameCheck <- checkNames(x, c(pvar, qvar, pervar, prodID))
  if (colNameCheck$result == FALSE) {
    stop(colNameCheck$message)
  }
  x <- checkTypes(x, pvar, qvar, pervar)
  timeCheck <- isContinuous(x[[pervar]])
  if (timeCheck$result == FALSE) {
    stop(paste("The time period variable is not continuous.",
               "Missing periods:", timeCheck$missing))
  }
  x <- x[order(x[[pervar]], x[[prodID]]), ]
  n <- max(x[[pervar]], na.rm = TRUE)
  plist <- matrix(1, nrow = n, ncol = 1)
  naElements <- character()
  if (tolower(output) == "chained" & !(tolower(chainMethod) ==
                                       "pop")) {
    switch(tolower(chainMethod), plspread = {
      similarityMatrix <- relativeDissimilarity(x, pvar = pvar,
                                                qvar = qvar, pervar = pervar, prodID = prodID,
                                                similarityMethod = "plspread")
    }, logquadratic = {
      similarityMatrix <- relativeDissimilarity(x, pvar = pvar,
                                                qvar = qvar, pervar = pervar, prodID = prodID,
                                                similarityMethod = "logquadratic")
    }, asymplinear = {
      similarityMatrix <- relativeDissimilarity(x, pvar = pvar,
                                                qvar = qvar, pervar = pervar, prodID = prodID,
                                                similarityMethod = "asymplinear")
    }, mixscale = {
      similarityMatrix <- mixScaleDissimilarity(x, pvar = pvar,
                                                qvar = qvar, pervar = pervar, prodID = prodID,
                                                ...)
    })
    links <- maximumSimilarityLinks(similarityMatrix)
  }
  if (tolower(output) == "fixedbase") {
    xt0 <- x[x[[pervar]] == 1, ]
  }
  for (i in 2:n) {
    if (tolower(output) == "chained" & tolower(chainMethod) ==
        "pop" | tolower(output) == "pop") {
      xt0 <- x[x[[pervar]] == i - 1, ]
    }
    else if (tolower(output) == "chained" & !(tolower(chainMethod) ==
                                              "pop")) {
      xt0 <- x[x[[pervar]] == links[links$xt == i, 2],
               ]
    }
    xt1 <- x[x[[pervar]] == i, ]
    if (sample == "matched") {
      xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),
                 ]
      xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),
                 ]
    }
    if (nrow(xt1) == 0) {
      plist[i, 1] <- NA
      naElements <- paste0(naElements, i, sep = ",")
    }
    else {
      p0 <- xt0[[pvar]]
      p1 <- xt1[[pvar]]
      q0 <- xt0[[qvar]]
      q1 <- xt1[[qvar]]
      switch(tolower(indexMethod), dutot = {
        plist[i, 1] <- dutot_t(p0, p1)
      }, carli = {
        plist[i, 1] <- carli_t(p0, p1)
      }, jevons = {
        plist[i, 1] <- jevons_t(p0, p1)
      }, harmonic = {
        plist[i, 1] <- harmonic_t(p0, p1)
      }, cswd = {
        plist[i, 1] <- cswd_t(p0, p1)
      }, laspeyres = {
        plist[i, 1] <- fixed_t(p0, p1, q0)
      }, paasche = {
        plist[i, 1] <- fixed_t(p0, p1, q1)
      }, fisher = {
        plist[i, 1] <- fisher_t(p0, p1, q0, q1)
      }, tornqvist = {
        plist[i, 1] <- tornqvist_t(p0, p1, q0, q1)
      }, satovartia = {
        plist[i, 1] <- satoVartia_t(p0, p1, q0, q1)
      }, walsh = {
        plist[i, 1] <- walsh_t(p0, p1, q0, q1)
      }, ces = {
        plist[i, 1] <- lloydMoulton_t0(p0, p1, q0, sigma = sigma)
      }, geomlaspeyres = {
        plist[i, 1] <- geomLaspeyres_t(p0, p1, q0, q1)
      }, geompaasche = {
        plist[i, 1] <- geomPaasche_t(p0, p1, q0, q1)
      })
      if (tolower(output) == "chained" & !(tolower(chainMethod) ==
                                           "pop")) {
        plist[i, 1] = plist[i, 1] * plist[links[links$xt ==
                                                  i, 2], 1]
      }
    }
  }
  if (tolower(output) == "chained" & tolower(chainMethod) ==
      "pop") {
    result <- apply(plist, 2, cumprod)
  }
  else {
    result <- plist
  }
  if (length(naElements) > 0) {
    warning(paste0("The following elements of the index were set to NA because there were no matched products in the two comparison periods: ",
                   naElements))
  }
  return(result)
}

