#' ---
#' title: # INSERT_REPORT_TITLE
#' author: # INSERT_AUTHOR
#' purpose: Store functions
#' start date: # YYYY-MM-DD
#' date modified: # YYYY-MM-DD        # CHANGE
#' Notes:                             # CHANGE
#' ---

# Install libraries -------------------------------------------------------------

# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!!!
PKG <- c(
  # For creating R Markdown Docs
  "knitr", # A general-purpose tool for dynamic report generation in R
  "rmarkdown", # R Markdown Document Conversion

  # File Management
  # "here", # For finding the root directory of your scripts and thus, find your files
  "officer",

  # Keeping Organized
  "devtools", # Package development tools for R; used here for downloading packages from GitHub
  # "renv", # saves the packages in the R environment


  # Graphics
  "ggplot2", # Create Elegant Data Visualisations Using the Grammar of Graphics
  # "nmfspalette",  # devtools::install_github("nmfs-general-modeling-tools/nmfspalette"
  "cowplot",
  "png",
  "extrafont",

  # Text
  "NMFSReports", # devtools::install_github("emilymarkowitz-noaa/NMFSReports") # Package of my favorite grammar and file managment functions for writing reproducible reports

  # Citations
  "knitcitations", # devtools::install_github("cboettig/knitcitations")

  # other tidyverse
  "dplyr",
  "magrittr",

  # Text Management
  "stringr",

  # For outputting JS files
  "jsonlite")


PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE)}
}

loadfonts(device = "win")

# renv ------------------------------------------------------------------

# renv::snapshot()


# Cite R Packages --------------------------------------------------------

knitr::write_bib(x = PKG,
                 file = paste0(dir_out_rawdata, "bibliography_RPack.bib"))

file.copy(from = paste0(dir_out_rawdata, "bibliography_RPack.bib"),
          to = paste0(dir_cite,"/bibliography_RPack.bib"),
          overwrite = TRUE)


# Housekeeping -----------------------------------------------------------------

# Keep chapter content in a proper order
cnt_chapt <- "000"
# Automatically name objects with consecutive numbers
cnt_figures <- 0 #  e.g., Figure 1
cnt_tables <- 0 # e.g., Table 1
cnt_equations <- 0 # e.g., Equation 1
# Save object content
list_equations <- list()
list_tables <- list()
list_figures <- list()

# Functions -------------------------------------------------------------

################Functions#############

