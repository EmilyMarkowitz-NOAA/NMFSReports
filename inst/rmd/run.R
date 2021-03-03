#' ---
#' title: # INSERT_REPORT_TITLE
#' author: # INSERT_AUTHOR
#' purpose: Run Scripts and R Markdown Files
#' start date: # YYYY-MM-DD
#' date modified: # YYYY-MM-DD                                          # CHANGE
#' Notes:                                                               # CHANGE
#' ---

######START#######

######***KNOWNS#########
report_title <- # INSERT_REPORT_TITLE
report_authors <- # INSERT_AUTHOR
report_yr <- substr(x = Sys.Date(), start = 1, stop = 4)                # CHANGE
report_office_location <- " > [Office Location]"                          # CHANGE
# For example:
# "National Oceanic and Atmospheric Administration\n
# 1315 East-West Highway [bldg./room]\n
# Silver Spring, MD 20910"\n
report_office <- "" # For example: AFSC, NEFSC                          # CHANGE
report_num <- "###"                                                     # CHANGE
report_NOAA_leaders <- "U.S. Department of Commerce

Wynn Coggins, Acting Secretary


National Oceanic and Atmospheric Administration

Benjamin Friedman, Acting NOAA Administrator


National Marine Fisheries Service

Paul Doremus, Acting Assistant Administrator for Fisheries"

#######***WHAT KIND OF OUTPUT#######
#Is this for InDesign?
indesign_flowin <- FALSE

#######SOURCE SUPPORT SCRIPTS#############
library(here) # Other functions load in the 0_functions.R

# INSERT_SUPPORT_SCRIPTS

#######SAVE PACKAGES USED TO CREATE THIS REPORT#############
# renv::init()
# renv::snapshot()


######MAKE REPORT########
cnt_chapt <- "000" # Keep everything in a proper order
plot_list <- c() # This will help us by saving R-ready plots so we can easily go back and edit them if necessary.
table_list <- c() # This will help us by saving R-ready tables  so we can easily go back and edit them if necessary.
cnt_figures <- 0 # This will autoname your figures with consecutive numbers (e.g., Figure 1.)
cnt_tables <- 0 # This will autoname your tables with consecutive numbers (e.g., Table 1.)
cnt_equ <- 0

####### RUN EACH SECTION#############


# INSERT_SECTIONS


# INSERT_POWERPOINT



##### SAVE OTHER OUTPUTS#############

save(plot_list,
     file=paste0(dir_out_figures, "/report_plots.rdata"))

save(table_list,
     file=paste0(dir_out_tables, "/report_tables.rdata"))

########***MAKE MASTER DOCX################

#USE GUIDENCE FROM THIS LINK
#https://support.microsoft.com/en-us/help/2665750/how-to-merge-multiple-word-documents-into-one


###############***METADATA##################
# So we can
#    1. Go back and recreate this exactly with the libraries you used to create this script and
#    2. Cite the apropriate versions of the packages you used in your report
# More info here: https://rstudio.github.io/packrat/walkthrough.html

CreateMetadata(dir_out = paste0(dir_out_todaysrun, "/documentation"),
               title = paste0(report_title, " Metadata ", Sys.Date()))

# setwd(paste0(dir_out_todaysrun))
