#' #' ---
#' purpose: store functions you'll use throughout the report
#' author: Me, myself, and I (me.noaa.gov)
#' start date: YYYY-MM
#' Notes: This is cool data... 
#' ---

#############INSTALL PACKAGES##############
# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!!!
PKG <- c( 
  # For creating R Markdown Docs
  "knitr", # A general-purpose tool for dynamic report generation in R
  "rmarkdown", # R Markdown Document Conversion

  # File Management
  "here", # For finding the root directory of your scripts and thus, find your files
  "officer",
  
  # Keeping Organized
  "devtools", # Package development tools for R; used here for downloading packages from GitHub
  # "packrat", # Packrat is a dependency management system for R.
  "renv",

  
  # Graphics
  # "ggplot2", # Create Elegant Data Visualisations Using the Grammar of Graphics
  "nmfspalette",  # devtools::install_github("nmfs-general-modeling-tools/nmfspalette")

  
  
  # Text
  "RMarkReports", # devtools::install_github("emilyhmarkowitz/RMarkReports") # Package of my favorite grammar and file managment functions for writing reproducible reports
  
  # Citations
  "knitcitations", # devtools::install_github("cboettig/knitcitations")
  
  # tidyverse
  "tidyverse"
  )

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

# renv::snapshot()


#######CITE R PACKAGES###########
tmp <- tempfile(fileext=".bib")


a<-write.bibtex(entry = c(citation("knitr"),
                          citation("rmarkdown"),
                          citation("here"),
                          citation("officer"),
                          citation("devtools"),
                          citation("packrat"),
                          citation("tidyverse"),
                          citation("RMarkReports"), 
                          citation("nmfspalette"),
                          citation("knitcitations")),
                file = tmp)

write.bibtex(entry = a, file = paste0(dir_out_data, "bibliography_RPack.bib"))
write.bibtex(entry = a, file = paste0(dir_data,"/bibliography_RPack.bib"))

################Functions#############

