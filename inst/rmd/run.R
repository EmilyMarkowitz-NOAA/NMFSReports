#' ---
#' title: Report Template: Run Scripts and R Markdown Files
#' purpose: Run Scripts and R Markdown Files
#' author: Me, myself, and I (me.noaa.gov)
#' start date: YYYY-MM
#' Notes: I've updated this script several times, specifically... 
#' ---

######START#######

# Always start with a clean state by removing everything in your environment!
rm(list=ls())
# renv::init()

######***KNOWNS#########
maxyr <- 2021 # or the year of the report, for example

#######***WHAT KIND OF OUTPUT#######
#Is this for InDesign? 
designflowin <- FALSE

#######SOURCE SUPPORT SCRIPTS#############

# INSERT_SUPPORT_SCRIPTS

######MAKE REPORT########
cnt.chapt<-"000" # Keep everything in a proper order
plot.list<-c() # This will help us by saving R-ready plots so we can easily go back and edit them if necessary. 
cnt.figures<-0 # This will autoname your figures with consecutive numbers (e.g., Figure 1.)
cnt.tables<-0 # This will autoname your tables with consecutive numbers (e.g., Table 1.)
cnt.equ<-0

#######RUN EACH SECTION#############

# ######***EXAMPLE############
# cnt.chapt<-auto_counter(cnt.chapt) # The order of the chapter in the report
# cnt.chapt.content<-"001" # The order of the content in the report (e.g., figures, images, tables)
# filename0<-paste0(cnt.chapt, "_Example_") #Seperated because we'll need it inside the RMarkdown
# rmarkdown::render(paste0(dir.scripts, "/00_example.Rmd"),
#                   output_dir = dir.chapters,
#                   output_file = paste0(filename0, cnt.chapt.content, "_Text.docx"))


# INSERT_SECTIONS

save(plot.list, file=paste0(dir.output.todaysrun, "/plots/reportPlots"))

########***MAKE MASTER DOCX################

#USE GUIDENCE FROM THIS LINK
#https://support.microsoft.com/en-us/help/2665750/how-to-merge-multiple-word-documents-into-one


###############***METADATA##################
# So we can 
#    1. Go back and recreate this exactly with the libraries you used to create this script and 
#    2. Cite the apropriate versions of the packages you used in your report
# More info here: https://rstudio.github.io/packrat/walkthrough.html

CreateMetadata(dir.out = paste0(dir.output.todaysrun, "/metadata"), 
               title = paste0(title0, " Metadata ", Sys.Date()))

# setwd(paste0(dir.output.todaysrun))
