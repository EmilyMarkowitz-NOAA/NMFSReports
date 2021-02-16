#'  ---
#' title: # INSERT_REPORT_TITLE
#' author: # INSERT_AUTHOR
#' purpose: Create directories and short hands for those directories
#' start date: # YYYY-MM-DD
#' date modified: # YYYY-MM-DD        # CHANGE
#' Notes:                             # CHANGE
#' ---

#############SAVE FILE LOCATIONS###############
# Just in case you change the base name for any reason, it will change for anytime you load the files inside the folder, too! (e.g., if you have something against "scripts" being the name of the folder, just let the script know in one place aka right here). 
library(here)

# Directories to all of your current folders
dirs<-list.dirs(path = here(), full.names = FALSE)
dirs<-dirs[!grepl(pattern = "\\/", x = dirs)]
dirs<-dirs[!grepl(pattern = "\\..", x = dirs)]
dirs<-dirs[dirs != ""]
for (i in 1:length(dirs)) {
  assign(x = paste0("dir_", dirs[i]), 
         value = paste0(here(dirs[i]), "/"))
}

# Where we save everything
dir.output<-paste0(here::here(), "/output/")
dir_out_todaysrun<-paste0(dir.output, "/",Sys.Date(),"/")
dir.create(dir_out_todaysrun)

dirs <- c("chapters", "rawdata", "documentation", "code", "figures", "tables")
for (i in 1:length(dirs)) {
  if (dir.exists(dirs[i]) == FALSE) {
    dir.create(dirs[i])
  }
  assign(x = paste0("dir_out_", dirs[i]), value = paste0(dir_out_todaysrun, "/",dirs[i],"/"))
}

# If loading in InDesign, table and figure headers need to be their own .docx. Here's a file that will do that for you. 
# TableFigureHeader<-system.file("rmd", "TableFigureHeader.Rmd", package = "RMarkReports")

TableFigureHeader<-paste0(dir_code, "TableFigureHeader.Rmd")

options("citation_format" = "pandoc")

#######SAVE ALL R FILES USED###########
listfiles<-list.files(path = dir_code) 
listfiles0<-c(listfiles[grepl(pattern = "\\.r", 
                              x = listfiles, ignore.case = T)], 
              listfiles[grepl(pattern = "\\.docx", 
                              x = listfiles, ignore.case = T)])
listfiles0<-listfiles0[!(grepl(pattern = "~",ignore.case = T, x = listfiles0))]

for (i in 1:length(listfiles0)){
  file.copy(from = paste0(dir_code, listfiles0[i]), 
            to = paste0(dir_out_code, listfiles0[i]), 
            overwrite = T)
}


# #######REFERENCE WORD DOCUMENT###########
# file.copy(from = paste0(dir_code, refdoc),
#           to = paste0(dir_code, "word-styles-reference.docx"),
#           overwrite = TRUE)
# 
# #######CITATION STYLE###########
# file.copy(from = paste0(here(), "/cit/", cls),
#           to = paste0(here(), "/cit/", "cit.csl"),
#           overwrite = TRUE)
options("citation_format" = "pandoc")
