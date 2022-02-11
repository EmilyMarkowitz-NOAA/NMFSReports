#' ---
#' title: # INSERT_REPORT_TITLE
#' author: # INSERT_AUTHOR
#' purpose: Run Scripts and R Markdown Files
#' start date: # YYYY-MM-DD
#' date modified: # YYYY-MM-DD                                          # CHANGE
#' Notes:                                                               # CHANGE
#' ---

# START ------------------------------------------------------------------------

# *** REPORT KNOWNS ------------------------------------------------------------
report_title <- # INSERT_REPORT_TITLE
report_authors <- # INSERT_AUTHOR
report_yr <- substr(x = Sys.Date(), start = 1, stop = 4)            # SUGGESTION

# *** OUTPUT TYPE --------------------------------------------------------------
#Is this for InDesign?
indesign_flowin <- FALSE

# *** SOURCE SUPPORT SCRIPTS ---------------------------------------------------

# INSERT_SUPPORT_SCRIPTS

# *** RENV: SAVE PACKAGES USED TO CREATE THIS REPORT ---------------------------
# renv::init()
# renv::snapshot()

# *** SIGN INTO GOOGLE DRIVE----------------------------------------------------

# googledrive::drive_deauth()
# googledrive::drive_auth()
# 1

# RUN FIGURES, TABLES, & EQUATIONS FOR REPORT ----------------------------------

if (FALSE) {
  cnt_chapt_content<-"000"
  filename0<-paste0(cnt_chapt, "_")
  rmarkdown::render(paste0(dir_code, "/0_figtab.Rmd"),
                    output_dir = dir_out_ref,
                    output_file = paste0(filename0, cnt_chapt_content, ".docx"))

  save(list_figures, file=paste0(dir_out_figures, "/report_figures.rdata"))
  save(list_tables, file=paste0(dir_out_tables, "/report_tables.rdata"))
  save(list_equations, file=paste0(dir_out_tables, "/report_equations.rdata"))
}

load(file = paste0(dir_out_figures, "/report_figures.rdata"))
load(file = paste0(dir_out_tables, "/report_tables.rdata"))
load(file = paste0(list_equations, "/report_equations.rdata"))

# RUN EACH REPORT SECTION ------------------------------------------------------

# *** RUN EACH REPORT SECTION --------------------------------------------------

# INSERT_SECTIONS

# MAKE MASTER DOCX -------------------------------------------------------------

#USE GUIDENCE FROM THIS LINK
#https://support.microsoft.com/en-us/help/2665750/how-to-merge-multiple-word-documents-into-one

# SAVE METADATA ----------------------------------------------------------------

con <- file(paste0(dir_out_todaysrun, "metadata.log"))
sink(con, append=TRUE)
sessionInfo()
sink() # Restore output to console
# cat(readLines("notes.log"), sep="\n") # Look at the log

