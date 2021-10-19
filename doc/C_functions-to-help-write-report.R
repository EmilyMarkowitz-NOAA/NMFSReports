## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE, echo = TRUE, warning = FALSE, 
  comment = FALSE, collapse = TRUE, include = TRUE, 
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)


## ----libraries----------------------------------------------------------------
library(NMFSReports)
library(magrittr)
library(ggplot2)
library(png)
library(cowplot)
library(magick)

## ----housekeeping-------------------------------------------------------------
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

## ----ex_data------------------------------------------------------------------
# example data
dat <- data.frame(x = rnorm(n = 100), 
                  y = rnorm(n = 100), 
                  col = rep_len(x = c("a", "b"), length.out = 5))

Footnotes.list<-list("ExOfStandardFt" = "Wow, this project is so cool!")

types_of_vessels <- c("NOAA Vessel", "F/V Fishing Boat", "R/V University Vessel", "Private Charter")

## ----eq_pythag0---------------------------------------------------------------
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
subobj <- ifelse(exists("subobj"), subobj, FALSE) # create a subobj letter (1a)
newobj <- ifelse(exists("newobj"), newobj, TRUE) # force a new object number (1b-> 2a and 1 -> 2)
cnt_chapt_content<-NMFSReports::auto_counter(cnt_chapt_content)
cnt_equations<-ifelse(newobj, cnt_equations+1, cnt_equations)

if (subobj) {
  # if it contains letters
  if (newobj) {
    cnt_equations_sub <- letters[1]
  } else {
    cnt_equations_sub <- letters[which(letters == 
                                      gsub("[^a-zA-Z]", "", list_equations[length(list_equations)][[1]]$number))+1]
  }
} else {
  cnt_equations_sub <- ""
}

# Systematically save your plot with this function
list_equations<-NMFSReports::save_equations(
  equation = equation, 
  list_equations = list_equations, 
  header = ifelse(exists("header", mode = "character"), header, ""),
  footnote = unlist(ifelse(exists(x = "footnote", mode = "character"), list(footnote), "")), 
  alttext = ifelse(exists("alttext", mode = "character"), alttext, header),
  nickname = ifelse(exists("nickname", mode = "character"), nickname, filename0),
  cnt_chapt_content = ifelse(exists("cnt_chapt_content", mode = "character"), cnt_chapt_content, "001"), 
  cnt = paste0(cnt_equations, cnt_equations_sub), 
)

## ---- echo = FALSE------------------------------------------------------------

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("equation", "header", "footnote", "subobj", "newobj", #"nickname", 
                 "alttext")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i]) & !exists(remove_who0[i], mode = "function")){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## ---- echo = FALSE------------------------------------------------------------

# Don't Edit This:
subobj <- ifelse(exists("subobj"), subobj, FALSE) # create a subobj letter (1a)
newobj <- ifelse(exists("newobj"), newobj, TRUE) # force a new object number (1b-> 2a and 1 -> 2)
cnt_chapt_content<-NMFSReports::auto_counter(cnt_chapt_content)
cnt_equations<-ifelse(newobj, cnt_equations+1, cnt_equations)

if (subobj) {
  # if it contains letters
  if (newobj) {
    cnt_equations_sub <- letters[1]
  } else {
    cnt_equations_sub <- letters[which(letters == 
                                      gsub("[^a-zA-Z]", "", list_equations[length(list_equations)][[1]]$number))+1]
  }
} else {
  cnt_equations_sub <- ""
}

# Systematically save your plot with this function
list_equations<-NMFSReports::save_equations(
  equation = equation, 
  list_equations = list_equations, 
  header = ifelse(exists("header", mode = "character"), header, ""),
  footnote = unlist(ifelse(exists(x = "footnote", mode = "character"), list(footnote), "")), 
  alttext = ifelse(exists("alttext", mode = "character"), alttext, header),
  nickname = ifelse(exists("nickname", mode = "character"), nickname, filename0),
  cnt_chapt_content = ifelse(exists("cnt_chapt_content", mode = "character"), cnt_chapt_content, "001"), 
  cnt = paste0(cnt_equations, cnt_equations_sub), 
)

## ---- echo = FALSE------------------------------------------------------------

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("equation", "header", "footnote", "subobj", "newobj", #"nickname", 
                 "alttext")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i]) & !exists(remove_who0[i], mode = "function")){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## ----eq_newton----------------------------------------------------------------
equation = "F = G \\frac {m_1 m_2}{d^2}"
nickname = "Newton"
header = "Newton's Universal Law of Gravitation"

# save yo' stuff and do a lot of behind the scenes work
# alt: this does the same thing as calling "child = " in the chunk header
res <- knitr::knit_child(
  text = knitr::knit_expand(
    file = system.file("rmd/_child_save_eq.Rmd", package = "NMFSReports") ), 
  quiet = TRUE
)
# `r res `

## ----ex_plot0-----------------------------------------------------------------
header <- "Here is a figure!"
footnote<-c("A footnote for this figure!", "A second footnote for this figure!")
nickname <- "ex_plot"
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
subobj <- ifelse(exists("subobj"), subobj, FALSE) # create a subobj letter (1a)
newobj <- ifelse(exists("newobj"), newobj, TRUE) # force a new object number (1b-> 2a and 1 -> 2)
cnt_chapt_content<-NMFSReports::auto_counter(cnt_chapt_content)
cnt_figures<-ifelse(newobj, cnt_figures+1, cnt_figures)

if (subobj) {
  # if it contains letters
  if (newobj) {
    cnt_figures_sub <- letters[1]
  } else {
    cnt_figures_sub <- letters[which(letters == 
                                      gsub("[^a-zA-Z]", "", list_figures[length(list_figures)][[1]]$number))+1]
  }
} else {
  cnt_figures_sub <- ""
}

if (!(exists("table_raw"))) {
  table_raw <- NULL
}

# Systematically save your plot with this function
list_figures<-NMFSReports::save_figures(
  figure = figure, 
  raw = table_raw, 
  list_figures = list_figures, 
  header = ifelse(exists("header", mode = "character"), header, ""),
  footnotes = unlist(ifelse(exists("footnotes", mode = "character"), list(footnotes), "")), 
  alttext = ifelse(exists("alttext", mode = "character"), alttext, header),
  filename0 = ifelse(exists("filename0", mode = "character"), filename0, nickname), 
  nickname = ifelse(exists("nickname", mode = "character"), nickname, filename0),
  filename_desc = ifelse(exists("filename_desc", mode = "character"), filename_desc, nickname),
  cnt_chapt_content = ifelse(exists("cnt_chapt_content", mode = "character"), cnt_chapt_content, "001"), 
  width = ifelse(exists("width", mode = "numeric"), width, 6), 
  height = ifelse(exists("height", mode = "numeric"), height, 6),
  cnt = paste0(cnt_figures, cnt_figures_sub), 
  path = dir_out_figures)


## ---- echo = FALSE, fig.cap=ifelse(indesign_flowin %in% TRUE, list_figures[[length(list_figures)]]$alttext,  list_figures[[length(list_figures)]]$header)----

# Print or Don't Print Plot in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is 
# something you might like to toggle on and off. 
# Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) {

  if (!(exists("usePNGPDF"))) {
   list_figures[[length(list_figures)]]$figure # print plot in text
  } else if (usePNGPDF == "png") {
    knitr::include_graphics(path = paste0(dir_out_figures,
                                 list_figures[nickname][[1]]$filename,".png"))
    # cowplot::ggdraw() +
    #       cowplot::draw_image(image = paste0(dir_out_figures, list_figures[nickname][[1]]$filename,".png"))
  # } else if (usePNGPDF == "pdf") {
    # cowplot::ggdraw() +
    #   draw_image(magick::image_read_pdf(path = paste0(dir_out_figures, 
    #                              list_figures[nickname][[1]]$filename,".pdf"),
    #                              density = 600))
    # cowplot::ggdraw() +
    #       cowplot::draw_image(image = paste0(dir_out_figures, list_figures[nickname][[1]]$filename,".pdf"))
    # knitr::include_graphics(path = paste0(dir_out_figures,
    #                              list_figures[nickname][[1]]$filename,".pdf"))
  }
} else if (indesign_flowin %in% TRUE){ # for reports that need to be flowed into InDesign
  Title0 <- ifelse(indesign_flowin %in% TRUE, "", list_figures[[length(list_figures)]]$caption)
  rmarkdown::render(system.file("rmd/_TableFigureHeader.Rmd", package = "NMFSReports"),
                    quiet = TRUE,
                    output_dir = dir_chapters,
                    output_file = paste0(filename00,cnt_chapt_content,"_Title.docx"))
}

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("figure", "table_raw", "header", "footnotes", "subobj", "newobj", #"nickname", 
                 "filename_desc", "alttext", "width", "height")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i]) & !exists(remove_who0[i], mode = "function")){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## ---- echo = FALSE------------------------------------------------------------

# Don't Edit This:
subobj <- ifelse(exists("subobj"), subobj, FALSE) # create a subobj letter (1a)
newobj <- ifelse(exists("newobj"), newobj, TRUE) # force a new object number (1b-> 2a and 1 -> 2)
cnt_chapt_content<-NMFSReports::auto_counter(cnt_chapt_content)
cnt_figures<-ifelse(newobj, cnt_figures+1, cnt_figures)

if (subobj) {
  # if it contains letters
  if (newobj) {
    cnt_figures_sub <- letters[1]
  } else {
    cnt_figures_sub <- letters[which(letters == 
                                      gsub("[^a-zA-Z]", "", list_figures[length(list_figures)][[1]]$number))+1]
  }
} else {
  cnt_figures_sub <- ""
}

if (!(exists("table_raw"))) {
  table_raw <- NULL
}

# Systematically save your plot with this function
list_figures<-NMFSReports::save_figures(
  figure = figure, 
  raw = table_raw, 
  list_figures = list_figures, 
  header = ifelse(exists("header", mode = "character"), header, ""),
  footnotes = unlist(ifelse(exists("footnotes", mode = "character"), list(footnotes), "")), 
  alttext = ifelse(exists("alttext", mode = "character"), alttext, header),
  filename0 = ifelse(exists("filename0", mode = "character"), filename0, nickname), 
  nickname = ifelse(exists("nickname", mode = "character"), nickname, filename0),
  filename_desc = ifelse(exists("filename_desc", mode = "character"), filename_desc, nickname),
  cnt_chapt_content = ifelse(exists("cnt_chapt_content", mode = "character"), cnt_chapt_content, "001"), 
  width = ifelse(exists("width", mode = "numeric"), width, 6), 
  height = ifelse(exists("height", mode = "numeric"), height, 6),
  cnt = paste0(cnt_figures, cnt_figures_sub), 
  path = dir_out_figures)


## ---- echo = FALSE, fig.cap=ifelse(indesign_flowin %in% TRUE, list_figures[[length(list_figures)]]$alttext,  list_figures[[length(list_figures)]]$header)----

# Print or Don't Print Plot in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is 
# something you might like to toggle on and off. 
# Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) {

  if (!(exists("usePNGPDF"))) {
   list_figures[[length(list_figures)]]$figure # print plot in text
  } else if (usePNGPDF == "png") {
    knitr::include_graphics(path = paste0(dir_out_figures,
                                 list_figures[nickname][[1]]$filename,".png"))
    # cowplot::ggdraw() +
    #       cowplot::draw_image(image = paste0(dir_out_figures, list_figures[nickname][[1]]$filename,".png"))
  # } else if (usePNGPDF == "pdf") {
    # cowplot::ggdraw() +
    #   draw_image(magick::image_read_pdf(path = paste0(dir_out_figures, 
    #                              list_figures[nickname][[1]]$filename,".pdf"),
    #                              density = 600))
    # cowplot::ggdraw() +
    #       cowplot::draw_image(image = paste0(dir_out_figures, list_figures[nickname][[1]]$filename,".pdf"))
    # knitr::include_graphics(path = paste0(dir_out_figures,
    #                              list_figures[nickname][[1]]$filename,".pdf"))
  }
} else if (indesign_flowin %in% TRUE){ # for reports that need to be flowed into InDesign
  Title0 <- ifelse(indesign_flowin %in% TRUE, "", list_figures[[length(list_figures)]]$caption)
  rmarkdown::render(system.file("rmd/_TableFigureHeader.Rmd", package = "NMFSReports"),
                    quiet = TRUE,
                    output_dir = dir_chapters,
                    output_file = paste0(filename00,cnt_chapt_content,"_Title.docx"))
}

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("figure", "table_raw", "header", "footnotes", "subobj", "newobj", #"nickname", 
                 "filename_desc", "alttext", "width", "height")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i]) & !exists(remove_who0[i], mode = "function")){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## ----noaalogo-----------------------------------------------------------------

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


## ----ex_table0----------------------------------------------------------------

header <- "Here is a table!"
footnote<-"A footnote for this table!"
nickname <- "ex_table" # this is so you can refer to it later systematically
alttext <- "This is a 3-column table of random letters (in the first column) and numbers (in the second and third columns). "

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

table_print <- flextable::flextable(table_print) %>% 
    NMFSReports::theme_flextable_nmfstm(x = .)

# save yo' stuff and do a lot of behind the scenes work
# alt: check out the "child = " in this chunk header (which must stay empty)
# ```{r, child = system.file("rmd/_child_save_tab.Rmd", package = "NMFSReports")}
# ```

## ---- echo = FALSE------------------------------------------------------------

# Don't Edit This:
# appendix <- ifelse(exists("subobj"), subobj, FALSE) # is this being saved to the Appendix?
subobj <- ifelse(exists("subobj"), subobj, FALSE) # create a subobj letter (1a)
newobj <- ifelse(exists("newobj"), newobj, TRUE) # force a new object number (1b-> 2a and 1 -> 2)
cnt_chapt_content<-NMFSReports::auto_counter(cnt_chapt_content)
cnt_tables <- ifelse(newobj, cnt_tables+1, cnt_tables)

if (subobj) {
  # if it contains letters
  if (newobj) {
    cnt_tables_sub <- letters[1]
  } else {
    cnt_tables_sub <- letters[which(letters == 
                                      gsub("[^a-zA-Z]", "", list_tables[length(list_tables)][[1]]$number))+1]
  }
} else {
  cnt_tables_sub <- ""
}

if (!(exists("table_raw"))) {
  table_raw <- NULL
} else if (!(exists("table_print"))) {
  table_print <- NULL
}

# Systematically save your table with this function
list_tables<-NMFSReports::save_tables(
  table_raw = table_raw, 
  table_print = table_print,
  list_tables = list_tables, 
  header = ifelse(exists("header", mode = "character"), header, ""),
  footnotes = unlist(ifelse(exists("footnotes", mode = "character"), list(footnotes), "")), 
  alttext = ifelse(exists("alttext", mode = "character"), alttext, header),
  filename0 = ifelse(exists("filename0", mode = "character"), filename0, nickname), 
  nickname = ifelse(exists("nickname", mode = "character"), nickname, filename0),
  filename_desc = ifelse(exists("filename_desc", mode = "character"), filename_desc, nickname),  
  cnt_chapt_content = ifelse(exists("cnt_chapt_content", mode = "character"), cnt_chapt_content, "001"), 
  cnt = paste0(cnt_figures, cnt_tables_sub), 
  path = cnt_tables)


## ---- echo = FALSE, fig.cap=ifelse(indesign_flowin %in% FALSE & (list_tables[[length(list_tables)]]$alttext)!="", list_tables[[length(list_tables)]]$alttext, "")----

# Print or Don't Print Plot in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is 
# something you might like to toggle on and off. 
# Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) { 
  # print table in text
  if (exists("table_print")) {
    if ((class(table_print) %in% c("data.frame", "matrix"))){
      list_tables[[length(list_tables)]]$print %>% 
        knitr::kable(row.names = FALSE, booktabs = TRUE)
    } else {
      list_tables[[length(list_tables)]]$print
    }
  } else {
    if ((class(table_print) %in% c("data.frame", "matrix"))) {
      list_tables[[length(list_tables)]]$raw %>% 
        knitr::kable(row.names = FALSE, booktabs = TRUE)
    } else {
      list_tables[[length(list_tables)]]$raw
    }
  } 
} else if (indesign_flowin %in% TRUE) { # for reports that need to be flowed into InDesign 
  
  Title0 <- ifelse(indesign_flowin %in% TRUE, "", list_tables[[length(list_tables)]]$caption)
  
  rmarkdown::render(system.file("rmd/_TableFigureHeader.Rmd", package = "NMFSReports"), 
                    quiet = TRUE,
                    output_dir = dir_chapters, 
                    output_file = paste0(filename00, cnt_chapt_content,"_Title.docx"))
}

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("header", "footnotes", "subobj", "newobj", #"nickname", 
                 "filename_desc", "alttext", 
                 "table_raw", "table_print")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i]) & !exists(remove_who0[i], mode = "function")){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## ---- echo = FALSE------------------------------------------------------------

# Don't Edit This:
# appendix <- ifelse(exists("subobj"), subobj, FALSE) # is this being saved to the Appendix?
subobj <- ifelse(exists("subobj"), subobj, FALSE) # create a subobj letter (1a)
newobj <- ifelse(exists("newobj"), newobj, TRUE) # force a new object number (1b-> 2a and 1 -> 2)
cnt_chapt_content<-NMFSReports::auto_counter(cnt_chapt_content)
cnt_tables <- ifelse(newobj, cnt_tables+1, cnt_tables)

if (subobj) {
  # if it contains letters
  if (newobj) {
    cnt_tables_sub <- letters[1]
  } else {
    cnt_tables_sub <- letters[which(letters == 
                                      gsub("[^a-zA-Z]", "", list_tables[length(list_tables)][[1]]$number))+1]
  }
} else {
  cnt_tables_sub <- ""
}

if (!(exists("table_raw"))) {
  table_raw <- NULL
} else if (!(exists("table_print"))) {
  table_print <- NULL
}

# Systematically save your table with this function
list_tables<-NMFSReports::save_tables(
  table_raw = table_raw, 
  table_print = table_print,
  list_tables = list_tables, 
  header = ifelse(exists("header", mode = "character"), header, ""),
  footnotes = unlist(ifelse(exists("footnotes", mode = "character"), list(footnotes), "")), 
  alttext = ifelse(exists("alttext", mode = "character"), alttext, header),
  filename0 = ifelse(exists("filename0", mode = "character"), filename0, nickname), 
  nickname = ifelse(exists("nickname", mode = "character"), nickname, filename0),
  filename_desc = ifelse(exists("filename_desc", mode = "character"), filename_desc, nickname),  
  cnt_chapt_content = ifelse(exists("cnt_chapt_content", mode = "character"), cnt_chapt_content, "001"), 
  cnt = paste0(cnt_figures, cnt_tables_sub), 
  path = cnt_tables)


## ---- echo = FALSE, fig.cap=ifelse(indesign_flowin %in% FALSE & (list_tables[[length(list_tables)]]$alttext)!="", list_tables[[length(list_tables)]]$alttext, "")----

# Print or Don't Print Plot in Text
# You don't want to print this in the document if this text will be flowed into InDesign. 
# However, sometimes its nice to see everything all together, so this variable is 
# something you might like to toggle on and off. 
# Hense, FALSE = print here, TRUE = don't print here, just make the .pdf (coded above)
if (indesign_flowin %in% FALSE) { 
  # print table in text
  if (exists("table_print")) {
    if ((class(table_print) %in% c("data.frame", "matrix"))){
      list_tables[[length(list_tables)]]$print %>% 
        knitr::kable(row.names = FALSE, booktabs = TRUE)
    } else {
      list_tables[[length(list_tables)]]$print
    }
  } else {
    if ((class(table_print) %in% c("data.frame", "matrix"))) {
      list_tables[[length(list_tables)]]$raw %>% 
        knitr::kable(row.names = FALSE, booktabs = TRUE)
    } else {
      list_tables[[length(list_tables)]]$raw
    }
  } 
} else if (indesign_flowin %in% TRUE) { # for reports that need to be flowed into InDesign 
  
  Title0 <- ifelse(indesign_flowin %in% TRUE, "", list_tables[[length(list_tables)]]$caption)
  
  rmarkdown::render(system.file("rmd/_TableFigureHeader.Rmd", package = "NMFSReports"), 
                    quiet = TRUE,
                    output_dir = dir_chapters, 
                    output_file = paste0(filename00, cnt_chapt_content,"_Title.docx"))
}

# make sure you dont mistakenly name other files with these names
remove_who <- c()
remove_who0 <- c("header", "footnotes", "subobj", "newobj", #"nickname", 
                 "filename_desc", "alttext", 
                 "table_raw", "table_print")
for (i in 1:length(remove_who0)){
  if(exists(remove_who0[i]) & !exists(remove_who0[i], mode = "function")){
    remove_who <- c(remove_who, remove_who0[i])
  }
}
remove(list = remove_who)


## ----ex_table_foot------------------------------------------------------------

header <- "Here is a table!"
nickname <- "ex_table_foot" # this is so you can refer to it later systematically

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
               fonttype = "bold") #%>% # bold
  # knitr::kable(row.names = FALSE, booktabs = TRUE) #print table in text

# save yo' stuff and do a lot of behind the scenes work
# alt: this does the same thing as calling "child = " in the chunk header
res <- knitr::knit_child(
  text = knitr::knit_expand(
    file = system.file("rmd/_child_save_tab.Rmd", package = "NMFSReports")), 
  quiet = TRUE
)
# `r res `


