#'  ---
#' title: 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
#' author: 'L. Britt, E. H. Markowitz, E. J. Dawson, and R. Haehn'
#' purpose: Create directories and short hands for those directories
#' start date: 2021-09-01
#' date modified: 2021-09-01        # CHANGE
#' Notes:                             # CHANGE
#' ---

# SAVE FILE LOCATIONS ----------------------------------------------------------
# Just in case you change the base name for any reason, it will change for anytime you load the files inside the folder, too! (e.g., if you have something against "scripts" being the name of the folder, just let the script know in one place aka right here).

# Directories to all of your current folders
dir_in<-paste0(getwd(), "/")
dirs<-list.dirs(path = getwd(), full.names = FALSE)
dirs<-dirs[!grepl(pattern = "\\/", x = dirs)]
dirs<-dirs[!grepl(pattern = "\\..", x = dirs)]
dirs<-dirs[dirs != ""]
for (i in 1:length(dirs)) {
  assign(x = paste0("dir_", dirs[i]),
         value = paste0(dir_in, (dirs[i]), "/"))
}

# Where we save everything
dir.output<-paste0(dir_in, "/output/")
dir_out_todaysrun<-paste0(dir.output, "/",Sys.Date(),"/")
dir.create(dir_out_todaysrun)

dirs <- c("chapters", "rawdata", "documentation", "code", "figures", "tables", "cite")
for (i in 1:length(dirs)) {
  if (dir.exists(paste0(dir_out_todaysrun, dirs[i])) == FALSE) {
    dir.create(paste0(dir_out_todaysrun, "/", dirs[i]))
  }
  assign(x = paste0("dir_out_", dirs[i]), value = paste0(dir_out_todaysrun, "/",dirs[i],"/"))
}

# If loading in InDesign, table and figure headers need to be their own .docx. Here's a file that will do that for you.
# TableFigureHeader<-system.file("rmd", "TableFigureHeader.Rmd", package = "RMarkReports")

TableFigureHeader<-paste0(dir_code, "TableFigureHeader.Rmd")

options("citation_format" = "pandoc")

# SAVE ALL R FILES USED FOR EACH RUN -------------------------------------------
listfiles<-list.files(path = dir_code)
listfiles0<-c(listfiles[grepl(pattern = "\\.r",
                              x = listfiles, ignore.case = T)],
              listfiles[grepl(pattern = "\\.pptx",
                              x = listfiles, ignore.case = T)],
              listfiles[grepl(pattern = "\\.docx",
                              x = listfiles, ignore.case = T)])
listfiles0<-listfiles0[!(grepl(pattern = "~",ignore.case = T, x = listfiles0))]

for (i in 1:length(listfiles0)){
  file.copy(from = paste0(dir_code, listfiles0[i]),
            to = paste0(dir_out_code, listfiles0[i]),
            overwrite = T)
}

# SAVE ALL CITE FILES USED FOR EACH RUN ----------------------------------------
listfiles0<-list.files(path = dir_cite)
listfiles0<-listfiles0[!(grepl(pattern = "~",ignore.case = T, x = listfiles0))]

for (i in 1:length(listfiles0)){
  file.copy(from = paste0(dir_cite, listfiles0[i]),
            to = paste0(dir_out_cite, listfiles0[i]),
            overwrite = T)
}

# CITATION STYLE ---------------------------------------------------------------
options("citation_format" = "pandoc")
