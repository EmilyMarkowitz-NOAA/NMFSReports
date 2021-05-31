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
title = "My Awesome Report!"

## -----------------------------------------------------------------------------
authors = "Me, Myself, and I"

## -----------------------------------------------------------------------------
# list the sections (that you will have different rmd scripts for) in order and with no spaces
sections <- c("frontmatter", # This is a specific template that matches the NOAA Template
              "abstract", # This, and all others unless otherwise mentioned, come from the same plain-slate document but are appropriately named and linked up in the 'run' file. 
              "introduction", 
              "methods", 
              "results", 
              "discussion", 
              "endmatter" # This is a specific template that will document all of your citations throughout the report, the R packages you used to create this report. I'm biased, but please give credit where credit is due! There are also spots here to list people's ORCID numbers and acknowlegements. 
) 

## -----------------------------------------------------------------------------
csl = "bulletin-of-marine-science"
# it looks something like this:
csl0 <- base::readLines(system.file("cite","bulletin-of-marine-science.csl", package="NMFSReports"))
head(csl0)

## -----------------------------------------------------------------------------
csl = "bib_example"
# it looks something like this:
bib <- base::readLines(system.file("rmd","bib_example.bib", package="NMFSReports"))
bib

## ---- eval = FALSE------------------------------------------------------------
#  NMFSReports::buildReport(
#          sections = sections,
#          authors = authors,
#          title = title,
#          styles_reference_pptx = styles_reference_pptx,
#          styles_reference_docx = styles_reference_docx,
#          bibliography.bib = bibliography.bib,
#          csl = csl
#  )

## -----------------------------------------------------------------------------
library(NMFSReports)
# Input variables for buildReport()
sections = c("coverpage", # This is a specific template for a 1 page coverpage
             "history", 
             "studyimportance", 
             "actions", 
             "endmatter") # This is a specific template that will document all of your citations throughout the report, the R packages you used to create this report. I'm biased, but please give credit where credit is due! There are also spots here to list people's ORCID numbers and acknowlegements. 
authors = "Very important people"
title = "Shorter Report!"
styles_reference_pptx = "refpptx_nmfs"
styles_reference_docx = "refdoc_fisheries_economics_of_the_us"
bibliography.bib = "bib_example"
# Find citation styles at: https://github.com/citation-style-language/styles
csl0 <- read.delim(file = "https://raw.githubusercontent.com/citation-style-language/styles/master/american-fisheries-society.csl", header = FALSE, )
colnames(csl0)<-NULL
rownames(csl0)<-NULL
write.table(x = csl0, file = "csl.csl", 
            quote = FALSE, row.names = FALSE, col.names = FALSE)
csl = ("./csl.csl")
head(csl0)

## ---- eval = FALSE------------------------------------------------------------
#  # Run buildReport() function
#  NMFSReports::buildReport(
#          sections = sections,
#          authors = authors,
#          title = title,
#          styles_reference_pptx = styles_reference_pptx,
#          styles_reference_docx = styles_reference_docx,
#          bibliography.bib = bibliography.bib,
#          csl = csl
#  )

