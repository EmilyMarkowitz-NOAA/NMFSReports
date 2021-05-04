## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE, echo = TRUE, warning = FALSE, 
  comment = FALSE, collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)


## ----setup--------------------------------------------------------------------
library(NMFSReports)
library(here)
library(ggplot2)

## ---- eval=FALSE--------------------------------------------------------------
#  
#  library(NMFSReports)
#  
#  # Input variables for buildReport()
#  sections = c("frontmatter", "abstract", "introduction", "methods", "results",
#               "discussion", "endmatter")
#  authors = "Me, Myself, and I"
#  title = "Awesome Report!"
#  styles_reference_pptx = "refppt_nmfs"
#  styles_reference_docx = "refdoc_noaa_tech_memo"
#  bibliography.bib = "bib_example"
#  csl = "bulletin-of-marine-science"
#  
#  # Run buildReport() function
#  buildReport(
#          sections = sections,
#          support_scripts = support_scripts,
#          authors = authors,
#          title = title,
#          styles_reference_pptx = styles_reference_pptx,
#          styles_reference_docx = styles_reference_docx,
#          bibliography.bib = bibliography.bib,
#          csl = csl
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  source("./code/run.R")

## ---- eval=FALSE--------------------------------------------------------------
#  #' ---
#  #' title: ''
#  #' author: ''
#  #' purpose: Run Scripts and R Markdown Files
#  #' start date: 2021-03-03
#  #' date modified: 2021-03-03                                            # CHANGE
#  #' Notes:                                                               # CHANGE
#  #' ---
#  
#  ######START#######
#  
#  ######***KNOWNS#########
#  report_title <- 'Awesome Report!'
#  report_authors <- 'Me, Myself, and I'
#  report_yr <- substr(x = Sys.Date(), start = 1, stop = 4)                # CHANGE
#  report_office_location <- " > [Office Location]"                        # CHANGE
#  # For example:
#  # "National Oceanic and Atmospheric Administration\n
#  # 1315 East-West Highway [bldg./room]\n
#  # Silver Spring, MD 20910"\n
#  report_office <- "" # For example: AFSC, NEFSC                          # CHANGE
#  report_num <- "###"                                                     # CHANGE
#  report_NOAA_leaders <- "U.S. Department of Commerce
#  
#  Wynn Coggins, Acting Secretary
#  
#  
#  National Oceanic and Atmospheric Administration
#  
#  Benjamin Friedman, Acting NOAA Administrator
#  
#  
#  National Marine Fisheries Service
#  
#  Paul Doremus, Acting Assistant Administrator for Fisheries"
#  
#  #######***WHAT KIND OF OUTPUT#######
#  #Is this for InDesign?
#  indesign_flowin <- FALSE
#  
#  #######SOURCE SUPPORT SCRIPTS#############
#  library(here) # Other functions load in the 0_functions.R
#  
#  source(here::here('code', 'directories.R' ))
#  
#  source(here::here('code', 'functions.R' ))
#  
#  source(here::here('code', 'dataDL.R' ))
#  
#  source(here::here('code', 'data.R' ))
#  
#  
#  
#  #######SAVE PACKAGES USED TO CREATE THIS REPORT#############
#  # renv::init()
#  # renv::snapshot()
#  
#  
#  ######MAKE REPORT########
#  cnt_chapt <- "000" # Keep everything in a proper order
#  figure_list <- c() # This will help us by saving R-ready plots so we can easily go back and edit them if necessary.
#  table_list <- c() # This will help us by saving R-ready tables  so we can easily go back and edit them if necessary.
#  cnt_figures <- 0 # This will autoname your figures with consecutive numbers (e.g., Figure 1.)
#  cnt_tables <- 0 # This will autoname your tables with consecutive numbers (e.g., Table 1.)
#  cnt_equ <- 0
#  
#  ####### RUN EACH SECTION#############
#  
#  
#  
#    ############# 0 - Example ####################
#    cnt_chapt<-auto_counter(cnt_chapt)
#    cnt_chapt_content<-"001"
#    filename0<-paste0(cnt_chapt, "_example_")
#    rmarkdown::render(paste0(dir_code, "/0_example.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  
#  
#    ############# 1 - Frontmatter ####################
#    cnt_chapt<-auto_counter(cnt_chapt)
#    cnt_chapt_content<-"001"
#    filename0<-paste0(cnt_chapt, "_frontmatter_")
#    rmarkdown::render(paste0(dir_code, "/1_frontmatter.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  
#  
#    ############# 2 - Abstract ####################
#    cnt_chapt<-auto_counter(cnt_chapt)
#    cnt_chapt_content<-"001"
#    filename0<-paste0(cnt_chapt, "_abstract_")
#    rmarkdown::render(paste0(dir_code, "/2_abstract.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  
#  
#    ############# 3 - Introduction ####################
#    cnt_chapt<-auto_counter(cnt_chapt)
#    cnt_chapt_content<-"001"
#    filename0<-paste0(cnt_chapt, "_introduction_")
#    rmarkdown::render(paste0(dir_code, "/3_introduction.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  
#  
#    ############# 4 - Methods ####################
#    cnt_chapt<-auto_counter(cnt_chapt)
#    cnt_chapt_content<-"001"
#    filename0<-paste0(cnt_chapt, "_methods_")
#    rmarkdown::render(paste0(dir_code, "/4_methods.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  
#  
#    ############# 5 - Results ####################
#    cnt_chapt<-auto_counter(cnt_chapt)
#    cnt_chapt_content<-"001"
#    filename0<-paste0(cnt_chapt, "_results_")
#    rmarkdown::render(paste0(dir_code, "/5_results.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  
#  
#    ############# 6 - Discussion ####################
#    cnt_chapt<-auto_counter(cnt_chapt)
#    cnt_chapt_content<-"001"
#    filename0<-paste0(cnt_chapt, "_discussion_")
#    rmarkdown::render(paste0(dir_code, "/6_discussion.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  
#  
#    ############# 7 - Endmatter ####################
#    cnt_chapt<-auto_counter(cnt_chapt)
#    cnt_chapt_content<-"001"
#    filename0<-paste0(cnt_chapt, "_endmatter_")
#    rmarkdown::render(paste0(dir_code, "/7_endmatter.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  
#      ############# 8 - Presentation ####################
#      cnt_chapt<-auto_counter(cnt_chapt)
#      cnt_chapt_content<-"001"
#      filename0<-paste0(cnt_chapt, "_presentation_")
#      rmarkdown::render(paste0(dir_code, "/8_presentation.Rmd"),
#                        output_dir = dir_out_chapters,
#                        output_file = paste0(filename0, cnt_chapt_content, ".pptx"))
#  
#  
#  ##### SAVE OTHER OUTPUTS#############
#  
#  save(figure_list,
#       file=paste0(dir_out_figures, "/report_plots.rdata"))
#  
#  save(table_list,
#       file=paste0(dir_out_tables, "/report_tables.rdata"))
#  
#  ########***MAKE MASTER DOCX################
#  
#  #USE GUIDENCE FROM THIS LINK
#  #https://support.microsoft.com/en-us/help/2665750/how-to-merge-multiple-word-documents-into-one
#  
#  
#  ###############***METADATA##################
#  # So we can
#  #    1. Go back and recreate this exactly with the libraries you used to create this script and
#  #    2. Cite the apropriate versions of the packages you used in your report
#  # More info here: https://rstudio.github.io/packrat/walkthrough.html
#  
#  create_metadata(dir_out = paste0(dir_out_todaysrun, "/documentation"),
#                 title = paste0(report_title, " Metadata ", Sys.Date()))
#  
#  # setwd(paste0(dir_out_todaysrun))
#  
#  

