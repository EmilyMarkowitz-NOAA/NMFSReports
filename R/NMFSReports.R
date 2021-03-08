
############ BUILD THE TM #################

#' Build your intitial architecture for your new NOAA Tech Memo or Report
#'
#' @param sections a string of the different sections of your report. Sections must be listed in order. Default = c("frontmatter", "abstract", "introduction", "methods", "results", "discussion", "endmatter"). Note that "frontmatter" and "endmatter" both have specific templates, and all others are from a blank template. "endmatter" will document all of your citations throughout the report, the R packages you used to create this report. I'm biased, but please give credit where credit is due! There are also spots here to list authors's ORCID and acknowlegelments.
#' @param authors Default = "". Here, add all author's first and last name as it should appear in the report.You can change this later by editing this in the run.R file.
#' @param title Default = "". Here, put the title of your report. You can change this later by editing this in the run.R file.
#' @param styles_reference_pptx A style reference guide from a powerpoint document (.pptx). This pulls the styles from a powerpoint document where you have defined each style. Either use NULL to not have a presentation, a local document (insert full path to local document), or a pre-made templates ("refppt_nmfs"). Default = "refppt_nmfs". You can change this later by renaming the file in the code folder.
#' @param styles_reference_docx A style reference guide from a word document (.docx). This pulls the styles from a word document where you have defined each style. Either use a local document (insert full path to local document) or some of the pre-made templates ("refdoc_noaa_tech_memo" or "refdoc_fisheries_economics_of_the_us"). Default = "refdoc_noaa_tech_memo". You can change this later by renaming the file in the code folder.
#' @param bibliography.bib Either use a local document (.bib format; insert full "path") or the example file from the package ("bib_example"). Default = "bib_example". You can change this later by renaming the file in the cite folder.
#' @param csl Citation style. Either use a local document (insert full path to local document) or some of the pre-made templates ("bulletin-of-marine-science"). A NOAA TM citation style needs to be created, but until then, the default = "bulletin-of-marine-science". You can change this later by renaming the file in the cite folder. Find citation styles at: https://github.com/citation-style-language/styles
#' @export
#' @return complete initial architecture for your R Markdown Report.
buildReport<-function(sections = c("frontmatter",
                               "abstract",
                               "introduction",
                               "methods",
                               "results",
                               "discussion",
                               "endmatter",
                               "presentation"),
                  authors = "",
                  title = "",
                  styles_reference_pptx = "refppt_nmfs",
                  styles_reference_docx = "refdoc_noaa_tech_memo",
                  bibliography.bib = "bib_example",
                  csl = "bulletin-of-marine-science") {

  ##################  Create Architecture
  dirs <- c("code", "data", "documentation", "img", "cite", "output")

  for (i in 1:length(dirs)) {
    if (dir.exists(dirs[i]) == FALSE) {
      dir.create(dirs[i])
    }
  }

  # Now... Load those folders with stuff you care about

  ################## RMD scripts
  a <- list.files(path = system.file("rmd", package="NMFSReports"), pattern = "0_")
  b <- c("example", sections)
  if (!(is.null(styles_reference_pptx))) {
    b <- c(b, "presentation")
  }
  counter<-NMFSReports::numbers0(x = c(0, length(b)))[1]
  temp<-gsub(pattern = "\\.Rmd", replacement = "",
             x = gsub(pattern = "0_", replacement = "",
                      x = a,
                      ignore.case = T))

  for (i in 1:length(b)){

    copyfrom<-ifelse((sum(temp %in% b[i]) == 1),
                     a[which(temp %in% b[i])],
                     "0_blank.Rmd")

    copyto <- paste0("./code/", counter,"_",b[i], ".Rmd")

    counter<-NMFSReports::auto_counter(counter)

    file.copy(from = system.file("rmd", copyfrom, package="NMFSReports"),
              to = copyto,
              overwrite = T)

    if (copyfrom %in% "0_blank.Rmd") {

      rfile <- base::readLines(paste0(copyto))

      rfile <- gsub(pattern = "SECTION_TITLE",
                    replacement = NMFSReports::TitleCase(b[i]),
                    x = rfile)

      utils::write.table(x = rfile,
                         file = copyto,
                         row.names = FALSE,
                         col.names = FALSE,
                         quote = FALSE)
    }
  }

  ################## R scripts
  a <- list.files(path = system.file("rmd", package="NMFSReports"), pattern = "1_")
  support_scripts = c("directories",
                      "functions",
                      "dataDL",
                      "data")
  b <- support_scripts
  for (i in 1:length(b)){

    temp<-gsub(pattern = "\\.R", replacement = "",
               x = gsub(pattern = "1_", replacement = "",
                        x = a,
                        ignore.case = T))

    copyfrom <- ifelse((sum(temp %in% b[i]) == 1),
                       a[which(temp %in% b[i])],
                       "1_blank.R")

    copyto <- paste0("./code/", b[i], ".R")

    file.copy(from = system.file("rmd", copyfrom, package="NMFSReports"),
              to = copyto,
              overwrite = T)

    rfile <- base::readLines(copyto)

    rfile <- gsub(pattern = "# INSERT_REPORT_TITLE",
                  replacement = ifelse(title %in% "", "''",
                                       paste0("'", title, "'")),
                  x = rfile)

    rfile <- gsub(pattern = "# INSERT_AUTHOR",
                  replacement = ifelse(authors %in% "", "''",
                                       paste0("'", authors, "'")),
                  x = rfile)

    rfile<-gsub(pattern = "# YYYY-MM-DD",
                replacement = Sys.Date(),
                x = rfile)

    utils::write.table(x = rfile,
                       file = copyto,
                       row.names = FALSE,
                       col.names = FALSE,
                       quote = FALSE)
  }

  ################## other content
  b<-c("header.yaml",
       styles_reference_docx,
       styles_reference_pptx,
       csl,
       "TableFigureHeader.Rmd",
       bibliography.bib) # bib_example

  for (i in 1:length(b)){

    b[i]<-dplyr::case_when(grepl(pattern = "refdoc", x = b[i]) ~
                             paste0(b[i], ".docx"),
                           grepl(pattern = "refppt", x = b[i]) ~
                             paste0(b[i], ".pptx"),
                           grepl(pattern = "bib_", x = b[i]) ~
                             paste0(b[i], ".bib"),
                           TRUE ~ b[i])

    if (system.file("rmd", b[i], package="NMFSReports") != "") { # is a file from the package
      copyfrom <- system.file("rmd", b[i], package="NMFSReports")
    } else if (system.file("cite", paste0(b[i], ".csl"), package="NMFSReports") != "") {
      copyfrom <- system.file("cite", paste0(b[i], ".csl"), package="NMFSReports")
    } else if (dir.exists(dirname(b[i]))) { # is a local path
      copyfrom <- b[i]
    }

    copyto <- dplyr::case_when(b[i] == paste0(styles_reference_docx, ".docx") ~
                                 "./code/styles_reference.docx",
                               b[i] == paste0(styles_reference_pptx, ".pptx") ~
                                 "./code/styles_reference.pptx",
                               b[i] == paste0(bibliography.bib, ".bib") ~
                                 "./cite/bibliography.bib",
                               b[i] == csl ~
                                 "./cite/citestyle.csl",
                               TRUE ~ paste0("./code/", b[i]))
    file.copy(from = copyfrom,
              to = copyto,
              overwrite = T)
  }

  ################## images
  # Load those folders with stuff you care about
  a<-list.files(path = system.file("img", package="NMFSReports"))
  for (i in 1:length(a)){
    file.copy(from = system.file("img", a[i], package="NMFSReports"),
              to = paste0("./img/", a[i]),
              overwrite = T)
  }

  ################## Write run.R
  run0 <- base::readLines(system.file("rmd","run.R",
                                      package="NMFSReports"))

  # directories

  # support_scripts
  a<-paste("source(here::here('code',",
           paste0("'", support_scripts, ".R'"),"))

", collapse = "")

  run0<-gsub(pattern = "# INSERT_SUPPORT_SCRIPTS",
             replacement = a,
             x = run0)

  # INSERT_SECTIONS
  b<-c("example", sections)
  counter<-NMFSReports::numbers0(x = c(0, length(b)))[1]
  sections_no<-NMFSReports::numbers0(c(0:length(sections), length(b)))
  sections_no<-sections_no[-length(sections_no)]

  a<-paste(paste0('
  ############# ', sections_no,' - ', stringr::str_to_title(b),' ####################
  cnt_chapt<-auto_counter(cnt_chapt)
  cnt_chapt_content<-"001"
  filename0<-paste0(cnt_chapt, "_', b,'_")
  rmarkdown::render(paste0(dir_code, "/',sections_no,'_',b,'.Rmd"),
                    output_dir = dir_out_chapters,
                    output_file = paste0(filename0, cnt_chapt_content, ".docx"))


  '), collapse = "")

  run0<-gsub(pattern = "# INSERT_SECTIONS",
             replacement = a,
             x = run0)


  # INSERT_POWERPOINT
  # only if there is a reference type specified
  if (!(is.null(styles_reference_pptx))) {
    sections_no_pres <- auto_counter(sections_no[length(sections_no)])
    a<-dplyr::if_else(is.na(styles_reference_pptx),
                      "",
                      paste(paste0('
    ############# ', sections_no_pres,' - Presentation ####################
    cnt_chapt<-auto_counter(cnt_chapt)
    cnt_chapt_content<-"001"
    filename0<-paste0(cnt_chapt, "_presentation_")
    rmarkdown::render(paste0(dir_code, "/',sections_no_pres,'_presentation.Rmd"),
                      output_dir = dir_out_chapters,
                      output_file = paste0(filename0, cnt_chapt_content, ".pptx"))


    '), collapse = ""))

    run0<-gsub(pattern = "# INSERT_POWERPOINT",
               replacement = a,
               x = run0)
  } else {
    run0<-gsub(pattern = "# INSERT_POWERPOINT",
               replacement = "",
               x = run0)
  }

  # OTHER CONTENT
  run0<-gsub(pattern = "# INSERT_REPORT_TITLE",
             replacement = ifelse(title %in% "", "''",
                                  paste0("'", title, "'")),
             x = run0)

  run0<-gsub(pattern = "# INSERT_AUTHOR",
             replacement = ifelse(authors %in% "", "''",
                                  paste0("'", authors, "'")),
             x = run0)

  run0<-gsub(pattern = "# YYYY-MM-DD",
             replacement = Sys.Date(),
             x = run0)


  # write new run file
  utils::write.table(x = run0,
                     file = "./code/run.R",
                     row.names = FALSE,
                     col.names = FALSE,
                     quote = FALSE)

  # done!

}





########## SEARCH STUFF ############


#' Is something in a matrix? Let's check!
#'
#' This function searches to see if item 'search_for' is within the matrix 'x' and returns a respective TRUE (T) and FALSE (F). This can be useful for adding footnotes, adding conditional text to your document, and much more!
#' @param x The matrix that needs to be searched.
#' @param search_for Items to be searched for in matrix x.
#' @keywords search, matrix, footnote, footnotes
#' @export
#' @return TRUE or FALSE
#' @examples
#' x = data.frame(matrix(1:9, nrow = 3, ncol = 3))
#' is_something_in_this_matrix(x,
#'                        search_for = 9)
#' x = data.frame(matrix(LETTERS[1:9], nrow = 3, ncol = 3))
#' is_something_in_this_matrix(x,
#'                        search_for = "J")
is_something_in_this_matrix<-function(x, search_for) {
  xx<-c()
  for (r in 1:nrow(x)) {
    if (is.na(search_for)) {
      xx<-c(xx, sum(is.na(x[r,]), na.rm = T))
    } else {
      xx<-c(xx, sum(x[r,] == search_for, na.rm = T))
    }
  }
  return(sum(xx)!=0) # This returns a logical T/F
}

########### CONVERT STUFF ###########

#' Convert dataframe to javascript
#'
#' Convert dataframe to javascript matrix.
#' @param dat The data frame you want to add the footnote to.
#' @keywords data.frame, javascript, footnotes, footnote
#' @export
#' @examples
#' dat <- cbind.data.frame(matrix(LETTERS[1:8], nrow = 4),
#'                            matrix(1:8, nrow = 4))
#' df2js(dat = dat)
df2js<-function(dat) {

  if (sum(names(dat) %in% "Footnotes") != 0) {
    dat$Footnotes<-as.character(dat$Footnotes)
    dat$Footnotes[dat$Footnotes %in% c("", "[]")]<-"null"
  }
  # dat<-lapply(X = dat, FUN = as.character)
  for (col in 1:(ncol(dat)-1)){ #not footnotes
    dat[,col]<-as.character(dat[,col])
    for (row in 1:nrow(dat)){
      dat[row,col]<-trimws(dat[row,col])
      dat[row,col]<-gsub(pattern = "\\*", replacement = "", x = dat[row,col])
      dat[row,col]<-ifelse(is.na(dat[row,col]),  "NA", dat[row,col])
    }
  }
  dat<-rbind.data.frame(names(dat), dat)

  str0<-(jsonlite::toJSON(as.matrix(dat)))
  # str0<-gsub(pattern = "null", replacement = 'NA', str0)
  str0<-gsub(pattern = '"null"', replacement = 'null', str0)
  str0<-gsub(pattern = '""', replacement = '"', str0)

  str0<-gsub(pattern = '\\],\\[', replacement = '\\],
             \\[', x = str0)
  str0<-gsub(pattern = '\\]\\"', replacement = '\\]', x = str0)
  str0<-gsub(pattern = '\\"\\[', replacement = '\\[', x = str0)
  # str0<-gsub(pattern = "'", replacement = "/'", x = str0)
  # str0<-gsub(pattern = "/[/'", replacement = "/[`", x = str0)
  # str0<-gsub(pattern = "'/]", replacement = "/]`", x = str0)
  # str0<-gsub(pattern = '\\[\"', replacement = '\\["', x = str0)

  # str0<-gsub(pattern = '\\\"', replacement = '"', x = str0, fixed = T)
  str0<-gsub(pattern = '\\\"', replacement = '', x = str0, fixed = T)
  str0<-gsub(pattern = '\\.\\]', replacement = '\\.\\"\\]', x = str0)

  return(str0)
}


############ MODIFY TEXT ################



#' Make a String Title Case
#'
#' Make a String Title Case (making and, the, an, etc. lower case)
#' @param str A string that you want to be in title case
#' @param add_dont_cap A vector of strings that the user does not want capitalized
#' @keywords Title, Case, word strings
#' @export
#' @examples
#' TitleCase("HelLo WoRLD OR good-bye?")
TitleCase <- function(str = "", add_dont_cap = "") {

  z <- strsplit(str, " ")[[1]]
  z <- paste(toupper(substring(z, 1,1)), substring(z, 2),
             sep="", collapse=" ")

  dontcap<-c( add_dont_cap, #user added
    # Which words should not be capitalized in a title?
    "a", "an", "the", # articles
    "for", "and", "nor", "but", "or", "yet", "so", # Coordinate conjunctions (FANBOYS).
    "at", "around", "by", "after", "along", "for", "from", "of", "on", "to", "with", "without") # Prepositions
  dontcap<-unique(dontcap)

  for (i in 1:length(dontcap)){
    whoisgoinglow<-which(tolower(strsplit(z, " ")[[1]]) %in% dontcap[i])

    # whoisgoinglow<-grep(pattern = paste0(dontcap[i]),
    #                     x = strsplit(z, " ")[[1]],
    #                     ignore.case = T)
    if (length(whoisgoinglow)!= 0 &&
        whoisgoinglow != 1) {
      z0<-tolower(strsplit(z, " ")[[1]][whoisgoinglow])
      z00<-strsplit(z, " ")[[1]]
      z00[whoisgoinglow]<-z0
      z<-paste(z00,sep="", collapse=" ")
    }
  }

  return(z)
}


#' Make a string lower case except for stated (and common NOAA) proper nouns.
#'
#' Make a string lower case except for stated (and common NOAA) proper nouns.
#' @param str0 The text string.
#' @param capitalizefirst Default = FALSE
#' @param add_cap A vector of strings that the user does not want capitalized
#' @keywords Text editing
#' @export
#' @examples
#' tolower2(str0 = "notice how there are built-in proper nouns are capitalized:
#' alaska is not in the south atlantic.",
#'          capitalizefirst = TRUE,
#'          add_cap = "Proper nouns")
tolower2<-function(str0,
                   capitalizefirst = FALSE,
                   add_cap = "") {
  str2<-c()

  if (str0[1] %in% "") {
    str<-""
  } else {
    for (i in 1:length(str0)) {
      str1<-gsub(pattern = "\\(", replacement = "\\( ", x = tolower(str0[i]))
      str1<-gsub(pattern = "\\)", replacement = " \\)", x = str1)
      str1<-strsplit(x = str1, split = " ")[[1]]
      # str1<-gsub(pattern = "fw", replacement = "freshwater", x = str1, ignore.case = T)

      keywords <- c( add_cap, #user added
        #State
        "Alabama", "Alaska", "California", "Connecticut",
        "Delaware", #"East Florida", "West Florida",
        "Florida", "Georgia",
        "Louisiana", "Maine", "Maryland", "Massachusetts",
        "Mississippi", "New Hampshire", "New Jersey", "New York",
        "North Carolina", "Oregon", "Rhode Island", "South Carolina",
        "Texas",  "Virginia", "Washington",
        #Region
        "North Pacific", "Pacific", "Western Pacific (Hawai`i)", "Western Pacific",
        "New England",
        "Mid-Atlantic","Gulf of Mexico",
        "South Atlantic",
        #For specific Species
        "Spanish", "Gulf", "Bringham's", "Von Siebold's", "Pfluger's", "African", "Eurpoean",
        "Southern kingfish", "Southern flounder",
        # Other
        "Atlantic", "American",
        "Atka", "Chinook", "Great Lakes")

      # keywords<-c(keywords, paste0("(", keywords), paste0(keywords, ")"))


      for (ii in 1:length(keywords)) {
        keywords1<-strsplit(x = keywords[ii], split = " ")[[1]]
        if (length(keywords1) %in% 1 &
            sum(grepl(x = str0, pattern = keywords1[1], ignore.case = T))>0) {
          str1[grep(x = str1, pattern = keywords[ii], ignore.case = T)]<-keywords[ii]
        } else if (length(keywords1) %in% 2 &
                   sum(grepl(x = str0, pattern = keywords1[1], ignore.case = T)>0) &
                   sum(grepl(x = str0, pattern = keywords1[2], ignore.case = T)>0)) {
          str1[grep(x = str1, pattern = keywords1[1], ignore.case = T)]<-keywords1[1]
          str1[grep(x = str1, pattern = keywords1[2], ignore.case = T)]<-keywords1[2]
        } else if (length(keywords1) %in% 3 &
                   grepl(x = str0, pattern = keywords1[1], ignore.case = T) &
                   grepl(x = str0, pattern = keywords1[2], ignore.case = T) &
                   grepl(x = str0, pattern = keywords1[3], ignore.case = T)) {
          str1[sum(grep(x = str1, pattern = keywords1[1], ignore.case = T)>0)]<-keywords1[1]
          str1[sum(grep(x = str1, pattern = keywords1[2], ignore.case = T)>0)]<-keywords1[2]
          str1[sum(grep(x = str1, pattern = keywords1[3], ignore.case = T)>0)]<-keywords1[3]
        }
      }

      # if (str1[1] == "von" & str1[2] == "siebolds") {
      #   str1<-str1[2:length(str1)]
      #   str1<-c("VonSiebold's", str1[3])
      # }

      # if (sum(grepl(pattern = "*A'u*", x = str1, ignore.case = T))>=1) {
      #   str1[grepl(pattern = "*A'u*", x = str1, ignore.case = T)]<-"*A\U02BBu*"
      # }
      #
      # if (sum(grepl(pattern = "*O'io*", x = str1, ignore.case = T))>=1) {
      #   str1[grepl(pattern = "*O'io*", x = str1, ignore.case = T)]<-"*O\U02BBio*"
      # }
      #
      # if (sum(grepl(pattern = "*'Ahi*", x = str1, ignore.case = T))>=1) {
      #   str1[grepl(pattern = "*'Ahi*", x = str1, ignore.case = T)]<-"*\U02BBAhi*"
      # }


      str1<-paste(str1, collapse = " ")
      str1<-gsub(pattern = "\\( ", replacement = "\\(", x = str1)
      str1<-gsub(pattern = " \\)", replacement = "\\)", x = str1)
      if (capitalizefirst==T) {
        str1<-paste(toupper(substr(str1, 1, 1)), substr(str1, 2, nchar(str1)), sep="")

      }

      str1<-gsub(pattern = "&", replacement = "and", x = str1)

      str2<-c(str2, str1)
    }
    str2<-trimws(str2)
  }
  return(str2)
}


#' Takes a string of words and combines them into a sentance that lists them.
#'
#' This function alows you to take a string of words and combine them into a sentance list. For example, 'apples', 'oranges', 'pears' would become 'apples, oranges, and pears'. This function uses oxford commas.
#' @param x Character strings you want in your string.
#' @param oxford T/F: would you like to use an oxford comma? Default = TRUE
#' @keywords strings
#' @export
#' @examples text_list(c(1,2,"hello",4,"world",6))
text_list<-function(x, oxford = TRUE) {
  x<-x[which(x!="")]
  # x<-x[which(!is.null(x))]
  x<-x[which(!is.na(x))]
  # x<-x[order(x)]
  if (length(x)==2) {
    str1<-paste(x, collapse = " and ")
  } else if (length(x)>2) {
    str1<-paste(x[1:(length(x)-1)], collapse = ", ")
    str1<-paste0(str1,
                 ifelse(oxford == TRUE, ",", ""),
                 " and ", x[length(x)])
  } else {
    str1<-x
  }
  return(str1)
}

#' Add footnotes within your tables in a smart way
#'
#' @param tab The data.frame you are adding footnotes to (and possibly from).
#' @param footnote A string that you want to add as a footnote to the table. Optional.
#' @param from_col What column number or name you want to pull footnotes from.
#' @param to_col What column number or name you want add footnotes to.
#' @param from_row What row number or name you want to pull footnotes from.
#' @param to_row What row number or name you want add footnotes to.
#' @param delim A deliminator string that seperates if you have multiple footnotes stored in a cell. The deliminator can be anything, as long as it does not conflict with anything that can be interpreted by regex. Default = "&&&"
#'
#' @return The table "tab" with the footnotes inserted into the table.
#' @export
#' @examples
#' table<-data.frame(col = LETTERS[1:10],
#'                       x = rnorm(n = 10),
#'                       y = rnorm(n = 10),
#'                       footnotes = NA)
#' table$footnotes[3]<-"Example footnote in a table 1."
#' table$footnotes[4]<-"Example footnote in a table 2.&&&Example footnote in a table 3."
#' table[,c("x", "y")] <- NMFSReports::mod_number(table[,c("x", "y")],
#'                                                      divideby = 1, #'
#'                                                      comma_seperator = TRUE,
#'                                                      digits = 2)
#' # Here, add footnotes from the "footnotes" column to the content in the first column where necessary
#' table <- add_table_footnotes(tab = table,
#'                                    from_col = "footnotes", # either use the name of the column
#'                                    to_col = 1) # or the number of that column in that table
#' # Here, add a specific footnote to a specific place in the table
#' table <- add_table_footnotes(tab = table,
#'                                    footnote = "Example footnote in a table 4.",
#'                                    to_row = 2,
#'                                    to_col = 2)
#' table <- add_table_footnotes(tab = table,
#'                                    footnote = c("Example footnote in a table 5.",
#'                                                 "Example footnote in a table 6."),
#'                                    to_row = 4,
#'                                    to_col = 2)
#' knitr::kable(table)
add_table_footnotes<-function(tab,
                              footnote = "",
                              from_col = "",
                              to_col = "",
                              from_row = "",
                              to_row = "",
                              delim = "&&&") {

  tab<-data.frame(tab)
  footnote <-trimws(footnote)

  idx <- function(tab, area, dimension) {
    idx0<-ifelse(area %in% "", list(1:(dim(tab)[dimension])), # all rows or columns
                 ifelse(is.numeric(area), area,
                        ifelse(is.character(area), which(names(tab) %in% area), "")))
    return(unlist(idx0))
  }

  from_col_idx <- idx(tab, area = from_col, dimension = 2)
  to_col_idx <- idx(tab, area = to_col, dimension = 2)
  from_row_idx <- idx(tab, area = from_row, dimension = 1)
  to_row_idx <- idx(tab, area = to_row, dimension = 1)

  if (sum(footnote %in% "") > 0) { # if pulling from somewhere in the table
    tab[from_row_idx, from_col_idx]<-trimws(tab[from_row_idx, from_col_idx])

    for (rr in 1:length(from_row_idx)) {
      for (cc in 1:length(from_col_idx)) {
        if (!(is.na(tab[from_row_idx[rr], from_col_idx[cc]]))) { # if there is a footnote
          tab[to_row_idx[rr], to_col_idx[cc]] <- paste0(tab[to_row_idx[rr], to_col_idx[cc]],
                                                        paste0(paste0("^[", strsplit(x = tab[from_row_idx[rr], from_col_idx[cc]],
                                                                                     split = delim)[[1]], "] "), collapse = "")) # otherwise, apend it
        }
      }
    }

  } else { # if adding footnote text directly to the table
    tab[to_row_idx, to_col_idx] <- paste0(tab[to_row_idx, to_col_idx],
                                          paste(paste0("^[", footnote, "] "), collapse = ""))
  }

  return(tab)

}

############ MODIFY NUMBERS IN TEXT ################


#' Convert number to text string.
#'
#' Function by John Fox found here: http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html and https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
#' @param x The numbers that need to be converted to string.
#' @keywords Text editing
#' @export
#' @examples
#' numbers2words(x = 1890)
#' numbers2words(x = 3)
#' numbers2words(x = 1800090)
numbers2words <- function(x){
  # Function by John Fox found here: http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html and https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
  if(x==0){
    print( "zero")
  } else{
    helper <- function(x){

      digits <- rev(strsplit(as.character(x), "")[[1]])
      nDigits <- length(digits)
      if (nDigits == 1) as.vector(ones[digits])
      else if (nDigits == 2)
        if (x <= 19) as.vector(teens[digits[1]])
      else trim(paste(tens[digits[2]],
                      Recall(as.numeric(digits[1]))))
      else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                                        Recall(makeNumber(digits[2:1]))))
      else {
        nSuffix <- ((nDigits + 2) %/% 3) - 1
        if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
        trim(paste(Recall(makeNumber(digits[
          nDigits:(3*nSuffix + 1)])),
          suffixes[nSuffix],"," ,
          Recall(makeNumber(digits[(3*nSuffix):1]))))
      }
    }
    trim <- function(text){
      #Tidy leading/trailing whitespace, space before comma
      text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
      #Clear any trailing " and"
      text=gsub(" and$","",text)
      #Clear any trailing comma
      gsub("\ *,$","",text)
    }
    makeNumber <- function(...) as.numeric(paste(..., collapse=""))
    #Disable scientific notation
    opts <- options(scipen=100)
    on.exit(options(opts))
    ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
              "eight", "nine")
    names(ones) <- 0:9
    teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
               "sixteen", " seventeen", "eighteen", "nineteen")
    names(teens) <- 0:9
    tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
              "ninety")
    names(tens) <- 2:9
    x <- round(x)
    suffixes <- c("thousand", "million", "billion", "trillion")
    if (length(x) > 1) return(trim(sapply(x, helper)))
    helper(x)
  }

}


#' Convert number to text string.
#'
#' Convert number to text string to the 'st, 'nd, 'rd, or 'th.
#' @param x The numbers that need to be converted to string.
#' @param type How the numbers should be converted. Default = "word" (which produces "fifty-third"), but you can also select "val_th" (which produces "53rd").
#' @keywords Text editing
#' @export
#' @examples
#' numbers2words_th(x = 3, type = "val_th")
#' numbers2words_th(x = 3, type = "word")
numbers2words_th<-function(x, type = "word"){

  # type = col name = c("val_th", "word")

  # First
  first2twen<-data.frame(matrix(data = c("first",	"1st",
                                         "second",	"2nd",
                                         "third",	"3rd",
                                         "fourth",	"4th",
                                         "fifth",	"5th",
                                         "sixth",	"6th",
                                         "seventh",	"7th",
                                         "eighth",	"8th",
                                         "ninth",	"9th",
                                         "tenth",	"10th",
                                         "eleventh",	"11th",
                                         "twelfth",	"12th",
                                         "thirteenth",	"13th",
                                         "fourteenth",	"14th",
                                         "fifteenth",	"15th",
                                         "sixteenth",	"16th",
                                         "seventeenth",	"17th",
                                         "eighteenth",	"18th",
                                         "nineteenth",	"19th",
                                         "twentieth",	"20th"), ncol = 2, byrow =  T))
  names(first2twen)<-c("word", "val_th")
  first2twen$val<-1:20

  # Tens
  tens<-data.frame(matrix(data = c("twentieth", 20,
                                   "thirtieth", 30,
                                   "fortieth",	40,
                                   "fiftieth",	50,
                                   "sixtieth",	60,
                                   "seventieth",	70,
                                   "eightieth",	80,
                                   "ninetieth",	90), ncol = 2, byrow =  T))
  names(tens)<-c("word", "val")
  tens$word0<-paste0(substr(x = tens$word, start = 1, stop = nchar(tens$word)-4), "y")
  tens$val_th<-paste0(tens$val, "th")

  # Hundred
  hund<-data.frame(matrix(data = c(
    "hundredth", 100,
    "thousandth", 1000,
    "millionth",	1000000,
    "billionth",	1000000000,
    "trillionth",	1000000000000), ncol = 2, byrow =  T))
  names(hund)<-c("word", "val")
  hund$word0<-paste0(substr(x = hund$word, start = 1, stop = nchar(hund$word)-2), "")
  tens$val_th<-paste0(tens$val, "th")

  if (x %in% 1:20) {
    xx<-first2twen[first2twen$val %in% x, type]
  } else if (substr(x = x, start = nchar(x), stop = nchar(x)) %in% 0) {
    xx<-tens[tens$val %in% round(x = x, digits = -1), type]
  } else {

    if (type %in% "word") {
      xx<-paste0(tens[tens$val %in% as.numeric(paste0(substr(x = x, start = 1, stop = 1), 0)), "word0"],
                 "-",
                 first2twen[(first2twen$val %in% (x-as.numeric(paste0(substr(x = x, start = 1, stop = 1), 0)))), type])
    } else {
      x1<-substr(x = x, start = nchar(x), stop = nchar(x))
      stndrdth<-"th"
      if (x1 %in% 1) {
        stndrdth<-"st"
      } else if (x1 %in% 2) {
        stndrdth<-"nd"
      } else if (x1 %in% 3) {
        stndrdth<-"rd"
      }
      xx<-paste0(x, stndrdth)

    }


  }

  return(xx)

}


#' Calculate the percent change.
#'
#' Calculate the percent change.
#' @param start The value it started with.
#' @param end The value it ended with.
#' @param ending A text string. Default "".
#' @param percent_first Options: T/F.
#' @keywords Modify number
#' @export
#' @examples
#' pchange(start = 8, end = 1)
#' pchange(start = 3, end = 6, ending = " in fish landings", percent_first = TRUE)
#' pchange(start = 3, end = 4, ending = " in fish landings", percent_first = FALSE)
pchange<-function(start, end, ending="", percent_first=TRUE){
  #calculate percent change:

  if (is.na(start) | is.na(end)) {

    final<-paste0(NA, "%")

  } else {

    start<-sum(as.numeric(start))
    end<-sum(as.numeric(end))

    p<-round(100*(end-start)/start)
    p<-ifelse(is.nan(p), 0, p)

    # decide direction, Omit if percent = 0:
    x<-p

    if (x<0) {
      txt<-paste0(" decrease",ending)
      p<-paste0("a ", abs(p),"%")
    } else if (x>0) {
      txt<-paste0(" increase",ending)
      p<-paste0("a ", abs(p),"%")
    } else if (round(x)==0){
      txt<-paste0("remains",ending," unchanged")
      p<-"" #ending must be "s" or "ed" here
    }

    # decide print order:
    if(percent_first) {
      final<-paste0(p,txt)
    } else {
      final<-paste0(txt," of ",p)
    }

    if (round(x)!=0) {
      if (sum(substr(x = numbers2words(abs(x)), start = 0, stop = 1) ==
              c("a", "e", "i", "o", "u"))==T & !(x %in% c(1, 100:199))) {
        final<-sub(pattern = "a ", replacement = "an ", x = final)
      }
    }
  }
  return(final)
}

#' Modify numbers.
#'
#' Modify numbers.
#' @param x A numeric.
#' @param divideby The value you want all of your values divided by. Default = 1000.
#' @param comma_seperator Do you want numbers to have commas in it ("1,000" (T) vs. "1000" (F). Default = TRUE.
#' @param digits How many digits you would like your number to have. Default = 0.
#' @keywords Modify number
#' @export
#' @examples
#' x = data.frame(matrix(data = c(20000678660, 234567, 1, NA, 2345, 23),
#'        ncol = 2))
#' mod_number(x)
#' mod_number(x,
#'        comma_seperator = FALSE)
#' x = data.frame(matrix(data = c(200000, 234567, 1, NA, 2345, 23)))
#' mod_number(x,
#'        divideby = 1,
#'        digits = 2)
mod_number<-function(x,
                 divideby = 1000,
                 comma_seperator = TRUE,
                 digits = 0) {
  xxx<-matrix(data = NA, nrow = nrow(x), ncol = ncol(x))

  for (c in 1:ncol(x)){
    for (r in 1:nrow(x)){
      xx<-ifelse(is.na(x[r,c]), NA,
                 as.numeric(gsub(x = x[r,c],
                                 pattern = ",",
                                 replacement = "")))
      # print(paste0(r,", ",c, ", ", xx))
      if (!is.na(xx)) {
          xx<-format(xx/divideby, digits = digits, trim = F,
                     big.mark = ifelse(comma_seperator == T, ",", ""),
                     scientific = F)
      }
      xxx[r,c]<-xx
    }}
  return(xxx)
}

#' Determine the appropriate unit for a value.
#'
#' Determine the appropriate unit for a value (e.g., 1000000 = '1 Million'.
#' @param value A numeric value.
#' @keywords Modify number, units
#' @export
#' @examples
#' xunits(value = c(12))
#' xunits(value = c(123456))
#' xunits(value = c(123456789))
xunits<-function(value
                 #, combine=TRUE # #' @param combine Should this be combined in a single string (T) or as two seperate strings in a list (F). Default = T.

                 ) {

  combine=TRUE

  out<-c()
  for (iii in 1:length(value)){
  value<-sum(as.numeric(value))
  if (is.na(value)) {
    out0<-NA
  } else {

    sigfig<-format(value, digits = 3, scientific = TRUE)
    sigfig0<-as.numeric(substr(x = sigfig, start = (nchar(sigfig)-1), stop = nchar(sigfig)))

    if (sigfig0<=5) {
      # if (sigfig0<4) {
      unit<-""
      x<-format(x = value, big.mark = ",", digits = 0, scientific = F)
      # } else if (sigfig0>=4 & sigfig0<6) {
      #   unit<-" thousand"
      # x<-round(value/1e3, digits = 1)
      # } else if (sigfig0==5) {
      #   unit<-" thousand"
      #   x<-round(value/1e3, digits = 0)
    } else if (sigfig0>=6 & sigfig0<9) {
      unit<-" million"
      x<-round(value/1e6, digits = 1)
    } else if (sigfig0>=9 & sigfig0<12) {
      unit<-" billion"
      x<-round(value/1e9, digits = 1)
    } else if (sigfig0>=12) {
      unit<-" trillion"
      x<-round(value/1e12, digits = 1)
    }
    out0<-ifelse(combine==T, paste0(x, unit), list(x, unit))
  }
  out<-c(out, out0)
  }

  return(out)
}

#' Determine the appropriate unit for a percent value.
#'
#' Determine the appropriate unit for a percent value (e.g., 1000000 = '1 Million'.
#' @param value A numeric.
#' @param sign Include percent sign. Default = T.
#' @keywords Modify number, units
#' @export
#' @examples
#' xunitspct(value = 8.4)
#' xunitspct(value = -8.4, sign = TRUE)
#' xunitspct(value = -8.4, sign = FALSE)
xunitspct<-function(value, sign = TRUE) {
  out0<-c()
  for (iii in 1:length(value)){

  if (is.na(value)) {
    temp<-NA
  } else if (value > -1 & value <= 0 | #negative values between 0 and -1
             value < 1 & value >= 0) { #positive values between 1 and 0
    temp<-as.numeric(format(value, digits = 0, nsmall = 1, big.mark = ",", trim =T, scientific = F))
  } else {
    temp<-as.numeric(round(value, digits = 0))
  }

  if (sign == F | is.na(value)) {
    out<-temp
  } else {
    out<-paste0(temp, "%")
  }
    out0<-c(out0, out)
  }

  return(out0)

}

#' Add bold, italics, strikethrough in formating to table.
#'
#' https://stackoverflow.com/questions/28166168/how-to-change-fontface-bold-italics-for-a-cell-in-a-kable-table-in-rmarkdown
#' @param dat A data.frame.
#' @param rows The rows you want to apply formatting to.
#' @param cols The columns you want to apply formatting to.
#' @param fonttype fonttype = c("italics", "bold", "strikethrough").
#' @keywords Modify number, units
#' @export
#' @examples
#' df <- data.frame(char = c('a','b','c'),
#'                  num = c(1,2,3))
#'
#' format_cells(df, 1, 1, "italics")
#' format_cells(df, 2, 2, "bold")
#' format_cells(df, 3, 1:2, "strikethrough")
#'
#' library(knitr)
#' library(kableExtra)
#' library(magrittr)
#' df %>%
#'   format_cells(1, 1, "italics") %>%
#'   format_cells(2, 2, "bold") %>%
#'   format_cells(3, 1:2, "strikethrough") %>%
#'   knitr::kable()
format_cells <- function(dat, rows, cols, fonttype) {
  # https://stackoverflow.com/questions/28166168/how-to-change-fontface-bold-italics-for-a-cell-in-a-kable-table-in-rmarkdown
  # select the correct markup
  map <- stats::setNames(c("*", "**", "~~"), c("italics", "bold", "strikethrough"))
  markup <- map[fonttype]

  for (r in rows){
    for(c in cols){

      # Make sure fonttypes are not factors
      dat[[c]] <- as.character( dat[[c]])

      # Update formatting
      dat[r, c] <- paste0(markup, dat[r, c], markup)
    }
  }

  return(dat)
}

######## FILE ORGANIZATION #########


#' Name nth item in order (001)
#'
#' Convert a value.
#' @param x a numeric or integer value or vector of numeric or integer values.
#' @keywords Data Management
#' @export
#' @return the values in the "0..X" format. All values will take on the number of 0s of the longest charcter value.
#' @examples
#' numbers0(x = c(1, 3, 6, 101))
numbers0<-function(x) {
  xx<-rep_len(x = NA, length.out = length(x))
  for (i in 1:length(x)){
    xx[i]<-paste0(paste(rep_len(x = 0,
                                length.out = nchar(max(x))-nchar(x[i])),
                        collapse = ""),
                  as.character(x[i]))
  }
  return(xx)
}

#' Add a counter number.
#'
#' Add a counter number, 1, 1+1=2, 2+1=3.
#' @param counter0 The value it was to be added 1 to
#' @keywords Data Management
#' @export
#' @return The number entered + 1, in the "0..X" format. All values will take on the number of 0s of the longest charcter value.
#' @examples
#' auto_counter(1)
auto_counter<-function(counter0) {
  counter00<-ifelse(as.numeric(counter0) %in% 0, 1, as.numeric(counter0)+1)
  counter<-numbers0(c(counter00, as.numeric(paste0("1",
                                                   paste(rep_len(x = 0, length.out = (nchar(counter0)-1)),
                                                         collapse = "")))))[1]
  return(counter)
}




####### TABLE AND GRAPHS #######


#' Systematically save your ggplot figure for your report
#'
#' @param plot0 The ggplot you would like to be saved
#' @param plot_list The list where all plots will be saved.
#' @param header The name and title of the figure. Default = "".
#' @param footnote Any footnote you want attached to this figure.
#' @param filename0 The filename set at the begining of the chapter
#' @param cnt_chapt_content The order number that this exists in the chapter
#' @param cnt The figure number
#' @param path The path the file needs to be saved to. Default = "NULL", meaning it wont save anything and will override all other saving elements.
#' @param width Default = 6 inches
#' @param height Default = 6 inches
#' @param output_type Default = c("pdf", "png"). Can be anything supported by ggsave()
#' @param type Default = "Figure", but can be anything that the element needs to be called (e.g., "Graphic", "Fig.", "Graph") to fit in the phrase "Figure 1. This is my plot!"
#' @param filename_desc Additional description text for the filename that will be added at the name of file before the filename extention. Can be use to add a species name, location, or anything else that would make it easier to know what that file shows.
#' @param message TRUE/FALSE. Default = FALSE. If TRUE, it will print information about where your plot has been saved to.
#' @importFrom magrittr %>%
#' @export
#' @return plot_list updated with the new plot and metadata.
#' @examples
#' library(magrittr)
#' library(ggplot2)
#' plot_list <- c()
#' dat <- data.frame(x = rnorm(n = 10),
#'                   y = rnorm(n = 10),
#'                   col = rep_len(x = c("a", "b"),
#'                                 length.out = 5))
#' # Select data and make plot
#' plot0<- dat %>%
#'   ggplot(aes(x = x, y = y,
#'   colour = as.factor(col))) + # create plot
#'   geom_point()
#' plot_list<-save_graphs(plot0 = plot0,
#'                       plot_list = plot_list,
#'                       header = "example",
#'                       footnote = "footnote example")
#' names(plot_list)
#' plot_list
save_graphs<-function(plot0,
                     plot_list,
                     header = "",
                     footnote = "",
                     filename0 = "x",
                     cnt_chapt_content = "001",
                     cnt = 1,
                     path = NULL,
                     width = 6,
                     height = 6,
                     output_type = c("pdf", "png"),
                     type = "Figure",
                     filename_desc = "",
                     message = FALSE){

  # Title
  header<-trimws(header)
  header<-stringr::str_to_sentence(header)
  header<-paste0(type, " ",cnt,". ",
                 ifelse(substr(x = header,
                               start = nchar(header),
                               stop = nchar(header)) %in%
                          c(".", "!", "?", "...", "...."),
                        header, paste0(header, ".")))
  footnote<-trimws(footnote)
  caption<-ifelse(footnote %in% "",
                 header,
                 paste0(header, "^[", footnote, "]"))
  filename00<-paste0(filename0, cnt_chapt_content, "_fig_",cnt,
                     ifelse(filename_desc!="", paste0("_", filename_desc), ""))

  # Save
  if (!is.null(path)){

    # Save Graphic/Figure
    for (i in 1:length(output_type)){
      ggplot2::ggsave( # save your plot
        path = path,
        filename = paste0(filename00, ".", output_type[i]), # Always save in pdf so you can make last minute edits in adobe acrobat!
      plot = plot0, # call the plot you are saving
      width = width, height = height, units = "in") #recall, A4 pages are 8.5 x 11 in - 1 in margins
    }
  }
  plot_list$temp <- list("plot" = plot0,
                              "caption" = caption,
                               "header" = header,
                               "footnote" = footnote)

  names(plot_list)[names(plot_list) %in% "temp"] <- header

  if (message == TRUE) {
    print(paste0("This figure was saved to ", path, filename00, ".*"))
  }


  return(plot_list)
}


#' Systematically save your report tables for your report
#'
#' @param table_raw Optional. The data.frame that has no rounding and no dividing of numbers (good to save this for record keeping). Default = NA.
#' @param table_print The data.frame as table will be seen in the report.
#' @param table_list Save tables in a list
#' @param header The name and title of the figure. Default = "".
#' @param footnote Any footnote you want attached to this figure.
#' @param filename0 The filename set at the begining of the chapter
#' @param cnt_chapt_content The order number that this exists in the chapter.
#' @param cnt The figure number
#' @param path The path the file needs to be saved to. Default = "NULL", meaning it wont save anything and will override all other saving elements.
#' @param output_type Default = c("csv"). Can be anything supported by utils::write.table.
#' @param type Default = "Table", but can be anything that the element needs to be called (e.g., "Graphic", "Fig.", "Graph") to fit in the phrase "Table 1. This is my spreadsheet!". Always save in pdf so you can make last minute edits in adobe acrobat!
#' @param filename_desc Additional description text for the filename that will be added at the name of file before the filename extention, before the "_raw" or "_print". Default = "". Can be use to add a species name, location, or anything else that would make it easier to know what that file shows.
#' @param message TRUE/FALSE. Default = FALSE. If TRUE, it will print information about where your plot has been saved to.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' # Select data and make plot
#' table_raw<-data.frame(x = rnorm(n = 10),
#'                       y = rnorm(n = 10),
#'                       col = rep_len(x = c("a", "b"), length.out = 5))
#' table_print <- table_raw
#' table_print[,c("x", "y")] <- NMFSReports::mod_number(table_print[,c("x", "y")],
#'                                                      divideby = 1,
#'                                                      comma_seperator = TRUE,
#'                                                      digits = 2)
#' save_tables(table_raw = table_raw,
#'            table_print=table_print,
#'            header = "Here is a table!",
#'            footnote = "A footnote for this table!")
save_tables<-function(table_raw = NULL,
                     table_print = NULL,
                     table_list = c(),
                     header = "",
                     footnote = "",
                     filename0 = "x",
                     cnt_chapt_content = "001",
                     cnt = 1,
                     path = NULL,
                     output_type = c("csv"),
                     type = "Table",
                     filename_desc = "",
                     message = FALSE) {


  # Title
  header<-trimws(header)
  header<-stringr::str_to_sentence(header)
  header<-paste0(type, " ",cnt,". ",
                 ifelse(substr(x = header,
                               start = nchar(header),
                               stop = nchar(header)) %in%
                          c(".", "!", "?", "...", "...."),
                        header, paste0(header, ".")))
  footnote<-trimws(footnote)
  caption<-ifelse(footnote %in% "",
                  header,
                  paste0(header, "^[", footnote, "]"))
  filename00<-paste0(filename0, cnt_chapt_content, "_tab_",cnt,
                     ifelse(filename_desc!="", paste0("_", filename_desc), ""))
  # Save
  if (!is.null(path)){

    # raw

    # Save raw file (no rounding, no dividing)
    if (!(is.null(table_raw))) {
      for (i in 1:length(output_type)){
        utils::write.table(x = table_raw,
                           file = paste0(path, filename00,
                                         "_raw.", output_type[i]),
                           sep = ",",
                           row.names=FALSE, col.names = F, append = F)
        }
    } else {
      table_raw <- ""
    }

    if (!(is.null(table_print))) {
      for (i in 1:length(output_type)){
        utils::write.table(x = table_print,
                           file = paste0(path, filename00,
                                         "_print.", output_type[i]),
                       sep = ",",
                       row.names=FALSE, col.names = F, append = F)
      }

    } else {
      table_print <- ""
    }
  }

  table_list$temp <- list("raw" = table_raw,
                          "print" = table_print,
                          "caption" = caption,
                          "header" = header,
                          "footnote" = footnote)

  names(table_list)[names(table_list) %in% "temp"] <- header

  if (message == TRUE) {
    print(paste0("This table was saved to ", path, filename00, ".*"))
  }
  return(table_list)

}


######## METADATA ########


#' Record Metadata
#'
#' Record Metadata
#' @param dir_out Path file will be saved to.
#' @param title Title of file.
#' @importFrom magrittr %>%
#' @keywords metadata
#' @export
CreateMetadata<-function(
  dir_out = ".",
  title = "My Project") {

  my_doc <- officer::read_docx()
  officer::styles_info(my_doc)

  my_doc <- my_doc %>%
    officer::body_add_par(title,
                 # officer::body_add_par(paste0("Population Narrative of ", commorg, " (", fp_text(sciname, italic = T, color = "black", font.size=10), ")"," in ", region),
                 style = "heading 1") %>%
    officer::body_add_par("Date Code Ran:", style = "heading 2") %>%
    officer::body_add_par(Sys.time(), style = "Normal") %>%
    officer::body_add_par("System Info:", style = "heading 2") %>%
    officer::body_add_par(paste0(Sys.info()[[1]], " ", R.version$platform), style = "Normal") %>%
    officer::body_add_par("R Version", style = "heading 2") %>%
    officer::body_add_par(paste0(R.version$version.string, ": ", R.version$nickname), style = "Normal") #%>%
  #   officer::body_add_par("Populations Run in this Iteration",
  #                style = "heading 2")
  # for (i in 1:length(org_pop)){
  #   my_doc <- my_doc %>%
  #     officer::body_add_par(org_pop[i], style = "Normal")
  # }

  a<-utils::sessionInfo()
  my_doc <- my_doc %>%
    officer::body_add_par("R Packages Loaded", style = "heading 2")
  for (i in 1:length(a$basePkgs)){
    my_doc <- my_doc %>%
      officer::body_add_par(a$basePkgs[i], style = "Normal")
  }
  for (i in 1:length(a$otherPkgs)){
    temp<-a$otherPkgs[[i]]
    my_doc <- my_doc %>%
      officer::body_add_par(temp$Package,
                   style = "heading 3") %>%
      officer::body_add_par(temp$Version, style = "Normal") %>%
      officer::body_add_par(temp$Title, style = "Normal") %>%
      officer::body_add_par(temp$Description, style = "Normal") %>%
      officer::body_add_par(temp$SystemRequirements, style = "Normal") %>%
      officer::body_add_par(paste0(temp$`Authors@R`), style = "Normal") %>%
      officer::body_add_par(temp$URL, style = "Normal")
  }

  print(my_doc, target = paste0(dir_out, "/Metadata_", Sys.Date(), ".docx"))
}



