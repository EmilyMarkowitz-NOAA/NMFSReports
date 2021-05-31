## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE, echo = TRUE, warning = FALSE, 
  comment = FALSE, collapse = TRUE, include = TRUE, 
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)


## ----setup--------------------------------------------------------------------
library(NMFSReports)
library(here)
library(magrittr)
library(ggplot2)
library(png)
library(cowplot)

## ----a------------------------------------------------------------------------
cnt_chapt <- "000" # Keep everything in a proper order
figure_list <- c() # This will help us by saving R-ready plots so we can easily go back and edit them if necessary. 
table_list <- c() # This will help us by saving R-ready tables  so we can easily go back and edit them if necessary. 
dir_out_figures <- dir_out_tables <- NULL # I dont really want to save them in this example
cnt_figures <- 0 # This will autoname your figures with consecutive numbers (e.g., Figure 1.)
cnt_tables <- 0 # This will autoname your tables with consecutive numbers (e.g., Table 1.)
cnt_equ <- 0
cnt_chapt_content<-0
filename0<-"example"
indesign_flowin<-FALSE # not going into a final publication that requires indesign for final touches

## ----ChunkName_NameMeAnythingAsLongAsItIsUnique-------------------------------
# Chunks are were we can write code for something later in the code. 

# example data
dat <- data.frame(x = rnorm(n = 100), 
                  y = rnorm(n = 100), 
                  col = rep_len(x = c("a", "b"), length.out = 5))

Footnotes.list<-list("ExOfStandardFt" = "Wow, this project is so cool!")

types_of_vessels <- c("NOAA Vessel", "F/V Fishing Boat", "R/V University Vessel", "Private Charter")

## ----Eq1----------------------------------------------------------------------
cnt_equ<-auto_counter(cnt_equ)

## ----Eq2----------------------------------------------------------------------
cnt_equ<-auto_counter(cnt_equ)

## ----G1a----------------------------------------------------------------------

header <- "Here is a figure!"
footnote<-c("A footnote for this figure!", "A second footnote for this figure!")
nickname <- "example_plot"
  
# Select data and make plot
figure <- dat %>%
  ggplot(aes(x = x, y = y, colour = as.factor(col))) + # create plot
  geom_point() 


## ---- echo = FALSE------------------------------------------------------------
# Don't Edit This:
cnt_chapt_content<-auto_counter(cnt_chapt_content)
cnt_figures<-cnt_figures+1

# Systematically save your plot with this function
figure_list<-save_figures(figure = figure, 
                      figure_list = figure_list, 
                      header = ifelse(exists("header"), header, ""),
                      footnote = ifelse(exists("footnote"), footnote, ""), 
                      filename0 = ifelse(exists("filename0"), filename0, nickname), 
                      nickname = ifelse(exists("nickname"), nickname, filename0),
                      filename_desc = ifelse(exists("filename_desc"), filename_desc, ""),
                      cnt_chapt_content = cnt_chapt_content, 
                      cnt = cnt_figures, 
                      path = dir_out_figures)


## ---- echo = FALSE, fig.cap=ifelse(indesign_flowin %in% TRUE, "", figure_list[[length(figure_list)]]$caption)----
# Print or Don't Print Plot in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is 
# something you might like to toggle on and off. 
# Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) { 
  figure # print plot in text
} else if (indesign_flowin %in% TRUE){ # for reports that need to be flowed into InDesign 
  rmarkdown::render(system.file("rmd/_TableFigureHeader.Rmd", package = "NMFSReports"),#TableFigureHeader, 
                    quiet = TRUE,
                  output_dir = dir_chapters, 
                  output_file = paste0(filename00,cnt_chapt_content,"_Title.docx"))
}

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("header", "footnote", "filename0", "nickname", "filename_desc")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i])){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## ---- echo = FALSE------------------------------------------------------------
# Don't Edit This:
cnt_chapt_content<-auto_counter(cnt_chapt_content)
cnt_figures<-cnt_figures+1

# Systematically save your plot with this function
figure_list<-save_figures(figure = figure, 
                      figure_list = figure_list, 
                      header = ifelse(exists("header"), header, ""),
                      footnote = ifelse(exists("footnote"), footnote, ""), 
                      filename0 = ifelse(exists("filename0"), filename0, nickname), 
                      nickname = ifelse(exists("nickname"), nickname, filename0),
                      filename_desc = ifelse(exists("filename_desc"), filename_desc, ""),
                      cnt_chapt_content = cnt_chapt_content, 
                      cnt = cnt_figures, 
                      path = dir_out_figures)


## ---- echo = FALSE, fig.cap=ifelse(indesign_flowin %in% TRUE, "", figure_list[[length(figure_list)]]$caption)----
# Print or Don't Print Plot in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is 
# something you might like to toggle on and off. 
# Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) { 
  figure # print plot in text
} else if (indesign_flowin %in% TRUE){ # for reports that need to be flowed into InDesign 
  rmarkdown::render(system.file("rmd/_TableFigureHeader.Rmd", package = "NMFSReports"),#TableFigureHeader, 
                    quiet = TRUE,
                  output_dir = dir_chapters, 
                  output_file = paste0(filename00,cnt_chapt_content,"_Title.docx"))
}

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("header", "footnote", "filename0", "nickname", "filename_desc")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i])){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## ----G2a----------------------------------------------------------------------

header <- "Here is a figure!"
footnote<-c("A footnote for this figure!")
nickname <- "noaalogo"
filename_desc <- "noaalogo"

# Select data and make plot
figure <- 
  cowplot::ggdraw() +
  cowplot::draw_image(readPNG(here::here("inst", "img", "noaa-gray.png"))) 
#   cowplot::draw_image(readPNG(here::here("img", "noaa-gray.png"))) 

# save yo' stuff and do a lot of behind the scenes work
# alt: this does the same thing as calling "child = " in the chunk header
res <- knitr::knit_child(
  text = knitr::knit_expand(
    file = system.file("rmd/_child_save_fig.Rmd", package = "NMFSReports")),
  quiet = TRUE
)

## ----T1a----------------------------------------------------------------------

header <- "Here is a table!"
footnote<-"A footnote for this table!"
nickname <- "example1" # this is so you can refer to it later systematically

# Select data and make plot

# Create data-saver version of table that will be used to reference things later saved as backup
table_raw<-data.frame(col = LETTERS[1:10], 
                      x = rnorm(n = 10), 
                      y = rnorm(n = 10)) 

# Create pretty version of table that will go into report
table_print <- table_raw
table_print[,c("x", "y")] <- 
  NMFSReports::mod_number(table_print[,c("x", "y")], 
                          divideby = 1, 
                          comma_seperator = TRUE, 
                          digits = 2)

# Format table 
table_print <- table_print %>%
  format_cells(rows = 0, # make column names
               cols = 1:ncol(table_print), # for all columns
               fonttype = "bold") %>% # bold
  knitr::kable(row.names = FALSE, booktabs = TRUE) #print table in text


## ---- echo = FALSE------------------------------------------------------------
# Don't Edit This:
cnt_chapt_content<-auto_counter(cnt_chapt_content)
cnt_tables<-cnt_tables+1

# Systematically save your table with this function
table_list<-save_tables(table_raw = ifelse(exists("table_raw"), table_raw, NULL), 
                        table_print = ifelse(exists("table_print"), table_print, NULL),
                        table_list = table_list, 
                        header = ifelse(exists("header"), header, ""),
                        footnote = ifelse(exists("footnote"), footnote, ""), 
                        filename0 = ifelse(exists("filename0"), filename0, nickname), 
                        nickname = ifelse(exists("nickname"), nickname, filename0),
                        filename_desc = ifelse(exists("filename_desc"), filename_desc, ""),  
                        cnt_chapt_content = cnt_chapt_content, 
                        cnt = cnt_tables, 
                        path = dir_out_tables)


## ---- echo = FALSE, fig.cap = ifelse(indesign_flowin %in% TRUE, "", table_list[[length(table_list)]]$caption)----
# Print or Don't Print Plot in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is 
# something you might like to toggle on and off. 
# Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) { 
  if (exists("table_print")) {
    table_print # print table in text
  } else {
    table_raw
  }
} else if (indesign_flowin %in% TRUE){ # for reports that need to be flowed into InDesign 
  rmarkdown::render(system.file("rmd/_TableFigureHeader.Rmd", package = "NMFSReports"),#TableFigureHeader, 
                    quiet = TRUE,
                  output_dir = dir_chapters, 
                  output_file = paste0(filename00, cnt_chapt_content,"_Title.docx"))
}

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("header", "footnote", "filename0", "nickname", "filename_desc", 
                 "table_raw", "table_print")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i])){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## ---- echo = FALSE------------------------------------------------------------
# Don't Edit This:
cnt_chapt_content<-auto_counter(cnt_chapt_content)
cnt_tables<-cnt_tables+1

# Systematically save your table with this function
table_list<-save_tables(table_raw = ifelse(exists("table_raw"), table_raw, NULL), 
                        table_print = ifelse(exists("table_print"), table_print, NULL),
                        table_list = table_list, 
                        header = ifelse(exists("header"), header, ""),
                        footnote = ifelse(exists("footnote"), footnote, ""), 
                        filename0 = ifelse(exists("filename0"), filename0, nickname), 
                        nickname = ifelse(exists("nickname"), nickname, filename0),
                        filename_desc = ifelse(exists("filename_desc"), filename_desc, ""),  
                        cnt_chapt_content = cnt_chapt_content, 
                        cnt = cnt_tables, 
                        path = dir_out_tables)


## ---- echo = FALSE, fig.cap = ifelse(indesign_flowin %in% TRUE, "", table_list[[length(table_list)]]$caption)----
# Print or Don't Print Plot in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is 
# something you might like to toggle on and off. 
# Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) { 
  if (exists("table_print")) {
    table_print # print table in text
  } else {
    table_raw
  }
} else if (indesign_flowin %in% TRUE){ # for reports that need to be flowed into InDesign 
  rmarkdown::render(system.file("rmd/_TableFigureHeader.Rmd", package = "NMFSReports"),#TableFigureHeader, 
                    quiet = TRUE,
                  output_dir = dir_chapters, 
                  output_file = paste0(filename00, cnt_chapt_content,"_Title.docx"))
}

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("header", "footnote", "filename0", "nickname", "filename_desc", 
                 "table_raw", "table_print")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i])){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## ----T2a----------------------------------------------------------------------

header <- "Here is a table!"
footnote<-"A footnote for this table!"
nickname <- "example_foot" # this is so you can refer to it later systematically

# Select data and make plot

# Create data-saver version of table that will be used to reference things later saved as backup
table_raw<-data.frame(col = LETTERS[1:10], 
                      x = rnorm(n = 10), 
                      y = rnorm(n = 10), 
                      footnotes = NA) 

table_raw$footnotes[3]<-"Example footnote in a table 1."
table_raw$footnotes[4]<-"Example footnote in a table 2.&&&Example footnote in a table 3."

# Create pretty version of table that will go into report
table_print <- table_raw
table_print[,c("x", "y")] <- NMFSReports::mod_number(table_print[,c("x", "y")], 
                                                     divideby = 1, 
                                                     comma_seperator = TRUE, 
                                                     digits = 2)

# example of how to add footnotes from a column of footnotes
# here, we'll add footnotes from the "footnotes" column to the content in the first column, where necessary
table_print <- add_table_footnotes(tab = table_print, 
                                   from_col = "footnotes", # either use the name of the column
                                   to_col = 1) # or the number of that column in that table

# here, I'll add a specific footnote to a specific place in the table
table_print <- add_table_footnotes(tab = table_print, 
                                   footnote = "Example footnote in a table 4.", 
                                   to_row = 2, 
                                   to_col = 2)

table_print <- add_table_footnotes(tab = table_print, 
                                   footnote = c("Example footnote in a table 5.", 
                                                "Example footnote in a table 6."), 
                                   to_row = 4, 
                                   to_col = 2)

table_print$footnotes<-NULL # remove column from final table

# Format table 
table_print <- table_print %>%
  format_cells(rows = 0, # make column names
               cols = 1:ncol(table_print), # for all columns
               fonttype = "bold") %>% # bold
  knitr::kable(row.names = FALSE, booktabs = TRUE) #print table in text

# save yo' stuff and do a lot of behind the scenes work
# alt: this does the same thing as calling "child = " in the chunk header
res <- knitr::knit_child(
  text = knitr::knit_expand(
    file = system.file("rmd/_child_save_tab.Rmd", package = "NMFSReports")
  ),
  quiet = TRUE
)


