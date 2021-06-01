## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE, echo = TRUE, warning = FALSE, 
  comment = FALSE, collapse = TRUE, include = TRUE, 
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)


## -----------------------------------------------------------------------------
library(NMFSReports)
library(magrittr)
library(ggplot2)
library(png)
library(cowplot)
library(magick)

## -----------------------------------------------------------------------------
cnt_chapt <- "000" # Keep everything in a proper order
cnt_chapt_content <- 0
cnt_equations <- 0
cnt_tables <- 0
cnt_figures <- 0
list_equations <- list()
list_tables <- list()
list_figures <- list()
dir_out_figures <- dir_out_tables <- NULL # I dont actually want to save them in this example
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

## -----------------------------------------------------------------------------
equation = "c^2 = b^2 + a^2"
nickname = "pythagorean"
header = "Pythagorean theorem"
footnote = "footnote about how cool the pythagorean theorem is."
alttext = "The Pythagoras theorem is a mathematical law that states that the sum of squares of the lengths of the two short sides of the right triangle is equal to the square of the length of the hypotenuse."
  
# save yo' stuff and do a lot of behind the scenes work
# alt: check out the "child = " in this next chunk header (which must stay empty)
# ```{r, child = system.file("rmd/_child_save_eq.Rmd", package = "NMFSReports")}
# ```

## ---- echo = FALSE------------------------------------------------------------
# Don't Edit This:
cnt_chapt_content<-NMFSReports::auto_counter(cnt_chapt_content)
cnt_equations<-cnt_equations+1
# Systematically save your plot with this function
list_equations<-NMFSReports::save_equations(
  equation = equation, 
  list_equations = list_equations, 
  header = ifelse(exists("header"), header, ""),
  footnote = unlist(ifelse(exists("footnote"), list(footnote), "")), 
  alttext = ifelse(exists("alttext"), alttext, ""),
  nickname = ifelse(exists("nickname"), nickname, filename0),
  cnt_chapt_content = cnt_chapt_content, 
  cnt = cnt_equations)

## ---- echo = FALSE------------------------------------------------------------

# , fig.alt = ifelse(indesign_flowin %in% TRUE, "", figure_list[[length(figure_list)]]$alttext)

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("equation", "header", "footnote", "nickname", "alttext")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i])){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## ---- echo = FALSE------------------------------------------------------------
# Don't Edit This:
cnt_chapt_content<-NMFSReports::auto_counter(cnt_chapt_content)
cnt_equations<-cnt_equations+1
# Systematically save your plot with this function
list_equations<-NMFSReports::save_equations(
  equation = equation, 
  list_equations = list_equations, 
  header = ifelse(exists("header"), header, ""),
  footnote = unlist(ifelse(exists("footnote"), list(footnote), "")), 
  alttext = ifelse(exists("alttext"), alttext, ""),
  nickname = ifelse(exists("nickname"), nickname, filename0),
  cnt_chapt_content = cnt_chapt_content, 
  cnt = cnt_equations)

## ---- echo = FALSE------------------------------------------------------------

# , fig.alt = ifelse(indesign_flowin %in% TRUE, "", figure_list[[length(figure_list)]]$alttext)

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("equation", "header", "footnote", "nickname", "alttext")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i])){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## -----------------------------------------------------------------------------
equation = "F = G \\frac {m_1 m_2}{d^2}"
nickname = "Newton"
header = "Newton's Universal Law of Gravitation"

# save yo' stuff and do a lot of behind the scenes work
# alt: this does the same thing as calling "child = " in the chunk header
res <- knitr::knit_child(
  text = knitr::knit_expand(
    file = system.file("rmd/_child_save_eq.Rmd", package = "NMFSReports")),
  quiet = TRUE
)
# `r res `

## -----------------------------------------------------------------------------
header <- "Here is a figure!"
footnote<-c("A footnote for this figure!", "A second footnote for this figure!")
nickname <- "example_plot"
alttext <- "This is a scatter plot of random data."
  
# Select data and make plot
figure <- dat %>%
  ggplot(aes(x = x, y = y, colour = as.factor(col))) + # create plot
  geom_point() 

# save yo' stuff and do a lot of behind the scenes work
# alt: check out the "child = " in this next chunk header (which must stay empty)
# ```{r, child = system.file("rmd/_child_save_fig.Rmd", package = "NMFSReports")}
# ```

## ---- echo = FALSE------------------------------------------------------------
# Don't Edit This:
cnt_chapt_content<-NMFSReports::auto_counter(cnt_chapt_content)
cnt_figures<-cnt_figures+1
# Systematically save your plot with this function
list_figures<-NMFSReports::save_figures(
  figure = figure, 
  list_figures = list_figures, 
  header = ifelse(exists("header"), header, ""),
  footnote = unlist(ifelse(exists("footnote"), list(footnote), "")), 
  alttext = ifelse(exists("alttext"), alttext, ""),
  filename0 = ifelse(exists("filename0"), filename0, nickname), 
  nickname = ifelse(exists("nickname"), nickname, filename0),
  filename_desc = ifelse(exists("filename_desc"), filename_desc, ""),
  cnt_chapt_content = cnt_chapt_content, 
  width = ifelse(exists("width"), width, 6), 
  height = ifelse(exists("height"), height, 6),
  cnt = cnt_figures, 
  path = dir_out_figures)

## ---- echo = FALSE, fig.cap=ifelse(indesign_flowin %in% TRUE, "", list_figures[[length(list_figures)]]$caption)----

# , fig.alt = ifelse(indesign_flowin %in% TRUE, "", list_figures[[length(list_figures)]]$alttext)

# Print or Don't Print Plot in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is 
# something you might like to toggle on and off. 
# Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) {
  # list_figures[[length(list_figures)]]$
    figure # print plot in text
} else if (indesign_flowin %in% TRUE){ # for reports that need to be flowed into InDesign
  Title0 <- ifelse(indesign_flowin %in% TRUE, "", list_figures[[length(list_figures)]]$caption)
  rmarkdown::render(system.file("rmd/_TableFigureHeader.Rmd", package = "NMFSReports"),
                    quiet = TRUE,
                    output_dir = dir_chapters,
                    output_file = paste0(filename00,cnt_chapt_content,"_Title.docx"))
}

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("figure", "header", "footnote", "nickname", "filename_desc", "alttext")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i])){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## ---- echo = FALSE------------------------------------------------------------
# Don't Edit This:
cnt_chapt_content<-NMFSReports::auto_counter(cnt_chapt_content)
cnt_figures<-cnt_figures+1
# Systematically save your plot with this function
list_figures<-NMFSReports::save_figures(
  figure = figure, 
  list_figures = list_figures, 
  header = ifelse(exists("header"), header, ""),
  footnote = unlist(ifelse(exists("footnote"), list(footnote), "")), 
  alttext = ifelse(exists("alttext"), alttext, ""),
  filename0 = ifelse(exists("filename0"), filename0, nickname), 
  nickname = ifelse(exists("nickname"), nickname, filename0),
  filename_desc = ifelse(exists("filename_desc"), filename_desc, ""),
  cnt_chapt_content = cnt_chapt_content, 
  width = ifelse(exists("width"), width, 6), 
  height = ifelse(exists("height"), height, 6),
  cnt = cnt_figures, 
  path = dir_out_figures)

## ---- echo = FALSE, fig.cap=ifelse(indesign_flowin %in% TRUE, "", list_figures[[length(list_figures)]]$caption)----

# , fig.alt = ifelse(indesign_flowin %in% TRUE, "", list_figures[[length(list_figures)]]$alttext)

# Print or Don't Print Plot in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is 
# something you might like to toggle on and off. 
# Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) {
  # list_figures[[length(list_figures)]]$
    figure # print plot in text
} else if (indesign_flowin %in% TRUE){ # for reports that need to be flowed into InDesign
  Title0 <- ifelse(indesign_flowin %in% TRUE, "", list_figures[[length(list_figures)]]$caption)
  rmarkdown::render(system.file("rmd/_TableFigureHeader.Rmd", package = "NMFSReports"),
                    quiet = TRUE,
                    output_dir = dir_chapters,
                    output_file = paste0(filename00,cnt_chapt_content,"_Title.docx"))
}

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("figure", "header", "footnote", "nickname", "filename_desc", "alttext")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i])){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## -----------------------------------------------------------------------------

header <- "Here is a figure!"
footnote<-c("A footnote for this figure!")
nickname <- "noaalogo"
filename_desc <- "noaalogo"
alttext <- "The NOAA Meatball and text. "

# Select data and make plot
figure <- 
  cowplot::ggdraw() +
  cowplot::draw_image(readPNG(system.file("img/NOAA_Fisheries_logo_vertical.png", package = "NMFSReports")) )

# save yo' stuff and do a lot of behind the scenes work
# alt: this does the same thing as calling "child = " in the chunk header
res <- knitr::knit_child(
  text = knitr::knit_expand(
    file = system.file("rmd/_child_save_fig.Rmd", package = "NMFSReports")),
  quiet = TRUE
)
# `r res `


## -----------------------------------------------------------------------------

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
  knitr::kable() #print table in text

# save yo' stuff and do a lot of behind the scenes work
# alt: check out the "child = " in this chunk header (which must stay empty)
# ```{r, child = system.file("rmd/_child_save_tab.Rmd", package = "NMFSReports")}
# ```

## ---- echo = FALSE------------------------------------------------------------
# Don't Edit This:
cnt_chapt_content<-NMFSReports::auto_counter(cnt_chapt_content)
cnt_tables<-cnt_tables+1

# Systematically save your table with this function
list_tables<-NMFSReports::save_tables(
  table_raw = ifelse(exists("table_raw"), table_raw, NULL), 
  table_print = ifelse(exists("table_print"), table_print, NULL),
  list_tables = list_tables, 
  header = ifelse(exists("header"), header, ""),
  footnote = unlist(ifelse(exists("footnote"), list(footnote), "")), 
  alttext = ifelse(exists("alttext"), alttext, ""),
  filename0 = ifelse(exists("filename0"), filename0, nickname), 
  nickname = ifelse(exists("nickname"), nickname, filename0),
  filename_desc = ifelse(exists("filename_desc"), filename_desc, ""),  
  cnt_chapt_content = cnt_chapt_content, 
  cnt = cnt_tables, 
  path = dir_out_tables)


## ---- echo = FALSE------------------------------------------------------------

# Print or Don't Print Plot in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is 
# something you might like to toggle on and off. 
# Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) { 
  if (exists("table_print")) {
    # list_tables[[length(list_tables)]]$
      table_print # print table in text
  } else {
    # list_tables[[length(list_tables)]]$
      table_raw
  }
} else if (indesign_flowin %in% TRUE){ # for reports that need to be flowed into InDesign 
  
  Title0 <- ifelse(indesign_flowin %in% TRUE, "", list_tables[[length(list_tables)]]$caption)
  
  rmarkdown::render(system.file("rmd/_TableFigureHeader.Rmd", package = "NMFSReports"), 
                    quiet = TRUE,
                    output_dir = dir_chapters, 
                    output_file = paste0(filename00, cnt_chapt_content,"_Title.docx"))
}

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("header", "footnote", "nickname", "filename_desc", "alttext", 
                 "table_raw", "table_print")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i])){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## ---- echo = FALSE------------------------------------------------------------
# Don't Edit This:
cnt_chapt_content<-NMFSReports::auto_counter(cnt_chapt_content)
cnt_tables<-cnt_tables+1

# Systematically save your table with this function
list_tables<-NMFSReports::save_tables(
  table_raw = ifelse(exists("table_raw"), table_raw, NULL), 
  table_print = ifelse(exists("table_print"), table_print, NULL),
  list_tables = list_tables, 
  header = ifelse(exists("header"), header, ""),
  footnote = unlist(ifelse(exists("footnote"), list(footnote), "")), 
  alttext = ifelse(exists("alttext"), alttext, ""),
  filename0 = ifelse(exists("filename0"), filename0, nickname), 
  nickname = ifelse(exists("nickname"), nickname, filename0),
  filename_desc = ifelse(exists("filename_desc"), filename_desc, ""),  
  cnt_chapt_content = cnt_chapt_content, 
  cnt = cnt_tables, 
  path = dir_out_tables)


## ---- echo = FALSE------------------------------------------------------------

# Print or Don't Print Plot in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is 
# something you might like to toggle on and off. 
# Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) { 
  if (exists("table_print")) {
    # list_tables[[length(list_tables)]]$
      table_print # print table in text
  } else {
    # list_tables[[length(list_tables)]]$
      table_raw
  }
} else if (indesign_flowin %in% TRUE){ # for reports that need to be flowed into InDesign 
  
  Title0 <- ifelse(indesign_flowin %in% TRUE, "", list_tables[[length(list_tables)]]$caption)
  
  rmarkdown::render(system.file("rmd/_TableFigureHeader.Rmd", package = "NMFSReports"), 
                    quiet = TRUE,
                    output_dir = dir_chapters, 
                    output_file = paste0(filename00, cnt_chapt_content,"_Title.docx"))
}

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("header", "footnote", "nickname", "filename_desc", "alttext", 
                 "table_raw", "table_print")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i])){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## -----------------------------------------------------------------------------

header <- "Here is a table!"
nickname <- "example_foot" # this is so you can refer to it later systematically

# Select data and make plot

# Create data-saver version of table that will be used to reference things later saved as backup
table_raw<-data.frame(col = LETTERS[1:10], 
                      x = rnorm(n = 10), 
                      y = rnorm(n = 10), 
                      footnotes = NA) 

table_raw$footnotes[3]<-"Example footnote in a table 1."
table_raw$footnotes[4]<-"Example footnote in a table 2.,,,Example footnote in a table 3."

footnote<-"A table with footnotes!"
footnote<-c(footnote, 
            ifelse(NMFSReports::is_something_in_this_matrix(
  x = table_raw, 
  search_for = "J"), 
  "There is a 'J' in this matrix!", 
  ""))

# Create pretty version of table that will go into report
table_print <- table_raw
table_print[,c("x", "y")] <- NMFSReports::mod_number(table_print[,c("x", "y")], 
                                                     divideby = 1, 
                                                     comma_seperator = TRUE, 
                                                     digits = 2)

# example of how to add footnotes from a column of footnotes
# here, we'll add footnotes from the "footnotes" column to the content in the first column, where necessary
table_print <- NMFSReports::add_table_footnotes(table = table_print, 
                                   from_col = "footnotes", # either use the name of the column or number
                                   to_col = 1, # or the number of that column in that table
                                   delim = ",,,") 
table_print$footnotes<-NULL # remove column from final table

# here, I'll add a specific footnote to a specific place in the table
table_print <- NMFSReports::add_table_footnotes(table = table_print, 
                                   footnote = "Example footnote in a table 4.", 
                                   to_row = 2, 
                                   to_col = 2)

# apply multiple footnotes to multiple (same number) spots
table_print <- NMFSReports::add_table_footnotes(table = table_print, 
                                   footnote = c("Example footnote in a table 5.", 
                                                "Example footnote in a table 6."), 
                                   to_row = 2:3,
                                   to_col = 2)


# Format table 
table_print <- table_print %>%
  NMFSReports::format_cells(rows = 0, # make column names
               cols = 1:ncol(table_print), # for all columns
               fonttype = "bold") %>% # bold
  knitr::kable(row.names = FALSE, booktabs = TRUE) #print table in text

# save yo' stuff and do a lot of behind the scenes work
# alt: this does the same thing as calling "child = " in the chunk header
res <- knitr::knit_child(
  text = knitr::knit_expand(
    file = system.file("rmd/_child_save_tab.Rmd", package = "NMFSReports")),
  quiet = TRUE
)
# `r res `


