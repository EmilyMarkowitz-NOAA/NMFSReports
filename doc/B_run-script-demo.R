## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE, echo = TRUE, warning = FALSE, 
  comment = FALSE, collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)


## ----setup--------------------------------------------------------------------
library(NMFSReports)

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
#          report_authors = authors,
#          report_title = title,
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
#  # START ------------------------------------------------------------------------
#  
#  # *** REPORT KNOWNS ------------------------------------------------------------
#  report_title <- # INSERT_REPORT_TITLE
#  report_authors <- # INSERT_AUTHOR
#  report_yr <- substr(x = Sys.Date(), start = 1, stop = 4)            # SUGGESTION
#  
#  # *** OUTPUT TYPE --------------------------------------------------------------
#  #Is this for InDesign?
#  indesign_flowin <- FALSE
#  
#  # *** SOURCE SUPPORT SCRIPTS ---------------------------------------------------
#  
#  source('./code/directories.R' )
#  
#  source('./code/functions.R' )
#  
#  source('./code/dataDL.R' )
#  
#  source('./code/data.R' )
#  
#  # *** RENV: SAVE PACKAGES USED TO CREATE THIS REPORT ---------------------------
#  # renv::init()
#  # renv::snapshot()
#  
#  # *** SIGN INTO GOOGLE DRIVE----------------------------------------------------
#  
#  # googledrive::drive_deauth()
#  # googledrive::drive_auth()
#  # 1
#  
#  # MAKE REPORT ------------------------------------------------------------------
#  
#  # *** HOUSEKEEPING -------------------------------------------------------------
#  
#  # Keep chapter content in a proper order
#  cnt_chapt <- "000"
#  # Automatically name objects with consecutive numbers
#  cnt_figures <- 0 #  e.g., Figure 1
#  cnt_tables <- 0 # e.g., Table 1
#  cnt_equations <- 0 # e.g., Equation 1
#  # Save object content
#  list_equations <- list()
#  list_tables <- list()
#  list_figures <- list()
#  
#  # *** RUN EACH REPORT SECTION --------------------------------------------------
#  
#  # *** *** 0 - Example ----------------------------
#  cnt_chapt<-auto_counter(cnt_chapt)
#  cnt_chapt_content<-"001"
#  filename0<-paste0(cnt_chapt, "_example_")
#  rmarkdown::render(paste0(dir_code, "/0_example.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  # *** *** 1 - Frontmatter ----------------------------
#  cnt_chapt<-auto_counter(cnt_chapt)
#  cnt_chapt_content<-"001"
#  filename0<-paste0(cnt_chapt, "_frontmatter_")
#  rmarkdown::render(paste0(dir_code, "/1_frontmatter.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  # *** *** 2 - Abstract ----------------------------
#  cnt_chapt<-auto_counter(cnt_chapt)
#  cnt_chapt_content<-"001"
#  filename0<-paste0(cnt_chapt, "_abstract_")
#  rmarkdown::render(paste0(dir_code, "/2_abstract.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  # *** *** 3 - Introduction ----------------------------
#  cnt_chapt<-auto_counter(cnt_chapt)
#  cnt_chapt_content<-"001"
#  filename0<-paste0(cnt_chapt, "_introduction_")
#  rmarkdown::render(paste0(dir_code, "/3_introduction.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  # *** *** 4 - Methods ----------------------------
#  cnt_chapt<-auto_counter(cnt_chapt)
#  cnt_chapt_content<-"001"
#  filename0<-paste0(cnt_chapt, "_methods_")
#  rmarkdown::render(paste0(dir_code, "/4_methods.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  # *** *** 5 - Results ----------------------------
#  cnt_chapt<-auto_counter(cnt_chapt)
#  cnt_chapt_content<-"001"
#  filename0<-paste0(cnt_chapt, "_results_")
#  rmarkdown::render(paste0(dir_code, "/5_results.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  # *** *** 6 - Discussion ----------------------------
#  cnt_chapt<-auto_counter(cnt_chapt)
#  cnt_chapt_content<-"001"
#  filename0<-paste0(cnt_chapt, "_discussion_")
#  rmarkdown::render(paste0(dir_code, "/6_discussion.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  # *** *** 7 - Endmatter ----------------------------
#  cnt_chapt<-auto_counter(cnt_chapt)
#  cnt_chapt_content<-"001"
#  filename0<-paste0(cnt_chapt, "_endmatter_")
#  rmarkdown::render(paste0(dir_code, "/7_endmatter.Rmd"),
#                      output_dir = dir_out_chapters,
#                      output_file = paste0(filename0, cnt_chapt_content, ".docx"))
#  
#  # *** *** 8 - Presentation ----------------------------
#  cnt_chapt<-auto_counter(cnt_chapt)
#  cnt_chapt_content<-"001"
#  filename0<-paste0(cnt_chapt, "_presentation_")
#  rmarkdown::render(paste0(dir_code, "/8_presentation.Rmd"),
#                        output_dir = dir_out_chapters,
#                        output_file = paste0(filename0, cnt_chapt_content, ".pptx"))
#  
#  # SAVE OTHER OUTPUTS -----------------------------------------------------------
#  
#  save(list_figures,
#       file=paste0(dir_out_figures, "/report_figures.rdata"))
#  
#  save(list_tables,
#       file=paste0(dir_out_tables, "/report_tables.rdata"))
#  
#  save(list_equations,
#       file=paste0(dir_out_tables, "/report_equations.rdata"))
#  
#  # MAKE MASTER DOCX -------------------------------------------------------------
#  
#  #USE GUIDENCE FROM THIS LINK
#  #https://support.microsoft.com/en-us/help/2665750/how-to-merge-multiple-word-documents-into-one
#  
#  # SAVE METADATA ----------------------------------------------------------------
#  
#  con <- file(paste0(dir_out_todaysrun, "metadata.log"))
#  sink(con, append=TRUE)
#  sessionInfo()
#  sink() # Restore output to console
#  # cat(readLines("notes.log"), sep="\n") # Look at the log
#  

