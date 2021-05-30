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
figure <- dat %>%
  ggplot(aes(x = x, y = y, colour = as.factor(col))) + # create plot
  geom_point() 

# cat(knitr::asis_output(
#   knitr::knit_child(input  = system.file("rmd/_child_save_fig.Rmd", package = "NMFSReports"), 
#                     quiet=TRUE)))


## ----G2a, include = FALSE, comment = FALSE, message = FALSE-------------------
header <- "Here is a figure!"
footnote<-c("A footnote for this figure!")
filename_desc <- "noaalogo"

# Select data and make plot
figure <- 
  cowplot::ggdraw() +
  cowplot::draw_image(readPNG(here::here("inst", "img", "noaa-gray.png"))) 

# save yo' stuff and do a lot of behind the scenes work
# cat(knitr::asis_output(
#   knitr::knit_child(input  = system.file("rmd/_child_save_fig.Rmd", package = "NMFSReports"), 
#                     quiet=TRUE)))

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

# save yo' stuff and do a lot of behind the scenes work
# cat(knitr::asis_output(
#   knitr::knit_child(input  = system.file("rmd/_child_save_tab.Rmd", package = "NMFSReports"), 
#                     quiet=TRUE)))



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
               fonttype = "bold") # bold

# save yo' stuff and do a lot of behind the scenes work
# cat(knitr::asis_output(
#   knitr::knit_child(input  = system.file("rmd/_child_save_tab.Rmd", package = "NMFSReports"), 
#                     quiet=TRUE)))


