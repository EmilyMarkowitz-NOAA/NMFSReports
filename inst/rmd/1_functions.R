#' ---
#' title: Data Report: Select relevant species
#' purpose: List and select all of the relevant species for each survey
#' author: emily.markowitz AT noaa.gov
#' start date: 2020-10
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

#############SAVE FILE LOCATIONS###############
# Just in case you change the base name for any reason, it will change for anytime you load the files inside the folder, too! (e.g., if you have something against "scripts" being the name of the folder, just let the script know in one place aka right here). 
library(here)

# Where the files we will need are saved
dir.scripts<-paste0(here::here(), "/code/")

# Where we save everything
dir.output<-paste0(here::here(), "/output/")
dir.output.todaysrun<-paste0(dir.output, "/",Sys.Date(),"/")
dir.create(dir.output.todaysrun)
dir.chapters<-paste0(dir.output.todaysrun, "/chapters/")
dir.create(dir.chapters)
dir.rawdata<-paste0(dir.output.todaysrun, "/rawdata/")
dir.create(dir.rawdata)
dir.tables<-paste0(dir.output.todaysrun, "/tables/")
dir.create(dir.tables)
dir.create(paste0(dir.output.todaysrun, "/code/"))
dir.create(paste0(dir.output.todaysrun, "/plots/"))
dir.create(paste0(dir.output.todaysrun, "/metadata/"))

# If loading in InDesign, table and figure headers need to be their own .docx. Here's a file that will do that for you. 
# TableFigureHeader<-system.file("rmd", "TableFigureHeader.Rmd", package = "RMarkReports")

TableFigureHeader<-paste0(dir.scripts, "TableFigureHeader.Rmd")

#######CITE R PACKAGES###########
tmp <- tempfile(fileext=".bib")
# RPackageCitations<-list()
# for (i in 1:length(PKG)){
#   RPackageCitations<-paste(RPackageCitations,
#                             citation(PKG[i]))
# }
# write.bibtex(entry = RPackageCitations,
#              file=temp)


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

write.bibtex(entry = a, file = paste0(dir.output.todaysrun, "bibliography_RPack.bib"))
write.bibtex(entry = a, file = "./data/bibliography_RPack.bib", )


#######REFERENCE WORD DOCUMENT###########
file.copy(from = paste0(dir.scripts, refdoc), 
          to = paste0(dir.scripts, "word-styles-reference.docx"), 
          overwrite = TRUE)

#######CITATION STYLE###########
file.copy(from = paste0(here(), "/citationStyles/", cls), 
          to = paste0(here(), "/citationStyles/", "citationstyle.csl"), 
          overwrite = TRUE)
options("citation_format" = "pandoc")

#######SAVE ALL R FILES USED###########
listfiles<-list.files(path = dir.scripts) 
listfiles0<-c(listfiles[grepl(pattern = "\\.r", 
                              x = listfiles, ignore.case = T)], 
              listfiles[grepl(pattern = "\\.docx", 
                              x = listfiles, ignore.case = T)])
listfiles0<-listfiles0[!(grepl(pattern = "~",ignore.case = T, x = listfiles0))]

for (i in 1:length(listfiles0)){
  file.copy(from = paste0(dir.scripts, listfiles0[i]), 
            to = paste0(dir.output.todaysrun, "/code/", listfiles0[i]), 
            overwrite = T)
}

