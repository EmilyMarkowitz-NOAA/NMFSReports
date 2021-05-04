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
library(magrittr)
library(ggplot2)
library(png)
library(cowplot)

## ----a------------------------------------------------------------------------
cnt_chapt <- "000" # Keep everything in a proper order
figure_list <- c() # This will help us by saving R-ready plots so we can easily go back and edit them if necessary. 
table_list <- c() # This will help us by saving R-ready tables  so we can easily go back and edit them if necessary. 
cnt_figures <- 0 # This will autoname your figures with consecutive numbers (e.g., Figure 1.)
cnt_tables <- 0 # This will autoname your tables with consecutive numbers (e.g., Table 1.)
cnt_equ <- 0

cnt_chapt_content<-0
filename0<-"example"
indesign_flowin<-FALSE

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

## ----G1a, include = FALSE, comment = FALSE, message = FALSE-------------------
# Edit This:
header <- "Here is a figure!"
footnote<-c("A footnote for this figure!", "A second footnote for this figure!")
nickname <- "example_plot"
  
# Select data and make plot
plot0 <- dat %>%
  ggplot(aes(x = x, y = y, colour = as.factor(col))) + # create plot
  geom_point() 

## ----G1b----------------------------------------------------------------------
# Don't Edit This:
cnt_chapt_content<-NMFSReports::auto_counter(cnt_chapt_content)
cnt_figures<-cnt_figures + 1

# Systematically save your plot with this simple function
figure_list<-save_figures(figure = plot0, 
                      figure_list = figure_list, 
                      header = header,
                      footnote = footnote,
                      filename0 = filename0, 
                      cnt_chapt_content = cnt_chapt_content, 
                      cnt = cnt_figures, 
                      nickname = nickname,
                      # path = dir_out_figures, 
                      width = 6, # you can change this if you need to...
                      height = 6) # you can change this if you need to...


## ----G1c, fig.cap=ifelse(indesign_flowin %in% TRUE, "", figure_list[[length(figure_list)]]$caption)----
# Print or Don't Print Plot in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is 
# something you might like to toggle on and off. 
# Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) { 
  plot0 # print plot in text
} else if (indesign_flowin %in% TRUE){ # for reports that need to be flowed into InDesign 
  rmarkdown::render(TableFigureHeader, 
                    quiet = TRUE,
                  output_dir = dir_chapters, 
                  output_file = paste0(filename00,cnt_chapt_content,"_Title.docx"))
}

## ----G2a, include = FALSE, comment = FALSE, message = FALSE-------------------
# Edit This:
header <- "Here is a figure!"
footnote<-c("A footnote for this figure!")
nickname <- "noaalogo"

# Select data and make plot
plot0 <- 
  cowplot::ggdraw() +
  cowplot::draw_image(readPNG(here::here("inst", "img", "noaa-gray.png"))) 

## ----G2b----------------------------------------------------------------------
# Don't Edit This:
cnt_chapt_content<-auto_counter(cnt_chapt_content)
cnt_figures<-cnt_figures+1

# Systematically save your plot with this simple function
figure_list<-save_figures(figure = plot0, 
                      figure_list = figure_list, 
                      header = header,
                      footnote = footnote,
                      filename0 = filename0, 
                      cnt_chapt_content = cnt_chapt_content, 
                      cnt = cnt_figures, 
                      nickname = nickname,
                      # path = dir_out_figures, 
                      width = 1, # you can change this if you need to...
                      height = 1,  # you can change this if you need to...
                      filename_desc = "noaalogo")


## ----G2c, fig.cap=ifelse(indesign_flowin %in% TRUE, "", figure_list[[length(figure_list)]]$caption)----
# Print or Don't Print Plot in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is 
# something you might like to toggle on and off. 
# Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) { 
  plot0 # print plot in text
} else if (indesign_flowin %in% TRUE){ # for reports that need to be flowed into InDesign 
  rmarkdown::render(TableFigureHeader, 
                    quiet = TRUE,
                  output_dir = dir_chapters, 
                  output_file = paste0(filename00,cnt_chapt_content,"_Title.docx"))
}

## ----T1a----------------------------------------------------------------------
# Edit This:
header <- "Here is a table!"
footnote<-"A footnote for this table!"
nickname <- "example1" # this is so you can refer to it later systematically

# Select data and make plot
table_raw<-data.frame(col = LETTERS[1:10], 
                      x = rnorm(n = 10), 
                y = rnorm(n = 10)) 
table_print <- table_raw
table_print[,c("x", "y")] <- NMFSReports::mod_number(table_print[,c("x", "y")], 
                                                     divideby = 1, 
                                                     comma_seperator = TRUE, 
                                                     digits = 2)


## ----T1b----------------------------------------------------------------------
# Don't Edit This:
cnt_chapt_content<-auto_counter(cnt_chapt_content)
cnt_tables<-cnt_tables+1

# Systematically save your plot with this simple function
table_list<-save_tables(table_raw = table_raw, 
                        table_print = table_print, 
                        table_list = table_list, 
                        header = header,
                        footnote = footnote,
                        filename0 = filename0,
                        nickname = nickname,
                        cnt_chapt_content = cnt_chapt_content, 
                        cnt = cnt_tables#, 
                        # path = dir_out_tables
                        )

## ----T1c, fig.cap=ifelse(indesign_flowin %in% TRUE, "", table_list[[length(table_list)]]$caption)----

# Print or Don't Print Table in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is something you might like to toggle on and off. Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) {
  table_print %>%
          format_cells(rows = 0, # make column names
                       cols = 1:ncol(table_print), # for all columns
                       fonttype = "bold") %>% # bold
          knitr::kable(row.names = FALSE, booktabs = TRUE) #print table in text
}

## ----T2a----------------------------------------------------------------------
# Edit This:
header <- "Here is a table!"
footnote<-"A footnote for this table!"
nickname <- "example_foot"

# Select data
table_raw<-data.frame(col = LETTERS[1:10], 
                      x = rnorm(n = 10), 
                y = rnorm(n = 10), 
                footnotes = NA) 

table_raw$footnotes[3]<-"Example footnote in a table 1."
table_raw$footnotes[4]<-"Example footnote in a table 2.&&&Example footnote in a table 3."
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


## ----T2b----------------------------------------------------------------------
# Don't Edit This:
cnt_chapt_content<-auto_counter(cnt_chapt_content)
cnt_tables<-cnt_tables+1
nickname <- "example_foot" # this is so you can refer to it later systematically

# Systematically save your plot with this simple function
table_list<-save_tables(table_raw = table_raw, 
                        table_print = table_print, 
                        table_list = table_list, 
                        header = header,
                        footnote = footnote,
                        filename0 = filename0, 
                        nickname = nickname,
                        cnt_chapt_content = cnt_chapt_content, 
                        cnt = cnt_tables#, 
                        # path = dir_out_tables
                        )

## ----T2c, fig.cap=ifelse(indesign_flowin %in% TRUE, "", table_list[[length(table_list)]]$caption)----

# Print or Don't Print Table in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is something you might like to toggle on and off. Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) {
  table_print %>%
          format_cells(rows = 0, # make column names
                       cols = 1:ncol(table_print), # for all columns
                       fonttype = "bold") %>% # bold
          knitr::kable(row.names = FALSE, booktabs = TRUE) #print table in text
}

