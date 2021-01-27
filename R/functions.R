


##########SEARCH STUFF############


#' Is something in a matrix.
#'
#' This function searches to see if item 'searchfor' is within the matrix 'x' and returns a respective true (T) and false (F).
#' @param x The matrix that needs to be searched.
#' @param searchfor Items to be searched for in matrix x.
#' @keywords search, matrix, footnote, footnotes
#' @export
#' @examples
#' issomethinginthismatrix()
issomethinginthismatrix<-function(x, searchfor) {
  xx<-c()
  for (r in 1:nrow(x)) {
    if (is.na(searchfor)) {
      xx<-c(xx, sum(is.na(x[r,]), na.rm = T))
    } else {
      xx<-c(xx, sum(x[r,] == searchfor, na.rm = T))
    }
  }
  if (sum(xx)==0) {
    return(F)
  } else {
    return(T)
  }
}

###########CONVERT STUFF###########

#' Convert dataframe to javascript
#'
#' Convert dataframe to javascript matrix.
#' @param df.dat The data frame you want to add the footnote to.
#' @param minyr Minimum year being assessed.
#' @param maxyr Maxium year being assessed.
#' @keywords data.frame, javascript, footnotes, footnote
#' @export
#' @examples
#' funct_df2js()
funct_df2js<-function(df.dat, minyr, maxyr) {
  
  df.dat$Footnotes<-as.character(df.dat$Footnotes)
  df.dat$Footnotes[df.dat$Footnotes %in% c("", "[]")]<-"null"
  
  # df.dat<-lapply(X = df.dat, FUN = as.character)
  for (col in 1:(ncol(df.dat)-1)){ #not footnotes
    df.dat[,col]<-as.character(df.dat[,col])
    for (row in 1:nrow(df.dat)){
      df.dat[row,col]<-trimws(df.dat[row,col])
      df.dat[row,col]<-gsub(pattern = "\\*", replacement = "", x = df.dat[row,col])
      df.dat[row,col]<-ifelse(is.na(df.dat[row,col]),  "NA", df.dat[row,col])
    }
  }
  df.dat<-rbind.data.frame(names(df.dat), df.dat)
  
  str0<-(toJSON(as.matrix(df.dat)))
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
  
  
  #make year numeric
  if (sum(names(df.dat)=="Year")==1) {
    for (i in 1:length(minyr:maxyr)) {
      str0<-gsub(pattern = paste0("'", as.character(minyr:maxyr)[i], "'"),
                 replacement = as.character(minyr:maxyr)[i], x = str0)
    }
  }

  return(str0)
}


############MODIFY TEXT################



#' Make a String Title Case
#'
#' Make a String Title Case (making and, the, an, etc. lower case)
#' @param str A string that you want to be in title case
#' @keywords Title, Case, word strings
#' @export
#'
#' @examples
#' TitleCase()
TitleCase <- function(str) {
  
  z <- strsplit(str, " ")[[1]]
  z <- paste(toupper(substring(z, 1,1)), substring(z, 2),
             sep="", collapse=" ")
  
  dontcap<-c( # Which words should not be capitalized in a title?
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


#' Make a string lower case except for stated proper nouns. 
#'
#' Make a string lower case except for stated proper nouns. 
#' @param str0 The text string.
#' @param capitalizefirst Default = FALSE
#' @keywords Text editing
#' @export
#' @examples
#' tolower2()
tolower2<-function(str0, capitalizefirst=F) {
  str2<-c()
  
  if (str0[1] %in% "") { 
    str<-""
  } else {
    for (i in 1:length(str0)) {
      str1<-gsub(pattern = "\\(", replacement = "\\( ", x = tolower(str0[i]))
      str1<-gsub(pattern = "\\)", replacement = " \\)", x = str1)
      str1<-strsplit(x = str1, split = " ")[[1]]
      # str1<-gsub(pattern = "fw", replacement = "freshwater", x = str1, ignore.case = T)
      
      keywords <- c(
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

#' Make first letter of a string capitalized. 
#'
#' Make first letter of a string capitalized. 
#' @param x The text string.
#' @keywords Text editing
#' @export
#' @examples
#' firstup()
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' List items in sentance form. 
#' 
#' List items in sentance form. 
#' @param x The text strings
#' @keywords Text editing
#' @export
#' @examples
#' text_list()
text_list<-function(x) {
  x<-x[which(x!="")]
  # x<-x[which(!is.null(x))]
  x<-x[which(!is.na(x))]
  # x<-x[order(x)]
  if (length(x)==2) { 
    str1<-paste(x, collapse = " and ")
  } else if (length(x)>2) {
    str1<-paste(x[1:(length(x)-1)], collapse = ", ")
    str1<-paste0(str1, ", and ", x[length(x)])
  } else {
    str1<-x
  }
  return(str1)
}


#' Takes a string of words and combines them into a sentance that lists them.
#'
#' This function alows you to take a string of words and combine them into a sentance list. For example, 'apples', 'oranges', 'pears' would become 'apples, oranges, and pears'. This function uses oxford commas.
#' @param x Character strings you want in your string.
#' @keywords list, strings
#' @export
#' @examples ABandC(c(1,2,"hello",4,"world",6))
ABandC <- function(x) {
  x <- x[which(x != "")]
  # x<-x[which(!is.null(x))]
  x <- x[which(!is.na(x))]
  # x<-x[order(x)]
  if (length(x) == 2) {
    str1 <- paste(x, collapse = " and ")
  } else if (length(x) > 2) {
    str1 <- paste(x[1:(length(x) - 1)], collapse = ", ")
    str1 <- paste0(str1, ", and ", x[length(x)])
  } else {
    str1 <- x
  }
  return(str1)
}

############MODIFY NUMBERS IN TEXT################


#' Convert number to text string. 
#' 
#' Convert number to text string. 
#' @param x The numbers that need to be converted to string. 
#' @keywords Text editing
#' @export
#' @examples
#' numbers2words()
numbers2words <- function(x){
  # Fork of https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r  
  ## Function by John Fox found here:
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
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
#' numbers2words_th()
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
#' pchange()
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

#' Modify number. 
#' 
#' Modify number. 
#' @param x A numeric. 
#' @param divideby The value you want all of your values divided by. Default = 1000. 
#' @param commaseperator Do you want numbers to have commas in it ("1,000" (T) vs. "1000" (F). Default = T. 
#' @param digits How many digits you would like your number to have. Default = 0. 
#' @keywords Modify number
#' @export
#' @examples
#' modnum()
modnum<-function(x, divideby = 1000, commaseperator = T, digits = 0) { 
  xxx<-matrix(data = NA, nrow = nrow(x), ncol = ncol(x))
  
  for (c in 1:ncol(x)){
    for (r in 1:nrow(x)){
      xx<-ifelse(is.na(x[r,c]), NA, as.numeric(gsub(x = x[r,c], pattern = ",", replacement = "")))
      # print(paste0(r,", ",c, ", ", xx))
      if (!is.na(xx)) {
        if(xx>0 & xx<999) {
          xx<-"< 1"
        } else if (xx>=1000 & xx<1499) {
          xx<-"1"    
        } else {
          xx<-format(xx/divideby, digits = digits, trim = F, 
                     big.mark = ifelse(commaseperator == T, ",", ""), scientific = F)
        }
      }
      xxx[r,c]<-xx
    }}
  return(xxx)
}

#' Determine the appropriate unit for a value. 
#' 
#' Determine the appropriate unit for a value (e.g., 1000000 = '1 Million'. 
#' @param value A numeric value. 
#' @param combine Should this be combined in a single string (T) or as two seperate strings in a list (F). Default = T. 
#' @keywords Modify number, units
#' @export
#' @examples
#' xunits()
xunits<-function(value, combine=T) {
  
  value<-sum(as.numeric(value))
  if (is.na(value)) {
    out<-NA
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
    
    out<-ifelse(combine==T, paste0(x, unit), list(x, unit))
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
#' xunitspct()
xunitspct<-function(value, sign = T) {
  
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
  
  return(out)
  
}

#' Add bold, italics, strikethrough in formating to table. 
#' 
#' Add bold, italics, strikethrough in formating to table. 
#' @param df.dat A data.frame. 
#' @param rows The rows you want to apply formatting to. 
#' @param cols The columns you want to apply formatting to. 
#' @param fonttype fonttype = c("italics", "bold", "strikethrough"). 
#' @keywords Modify number, units
#' @export
#' @examples
#' format_cells()
format_cells <- function(df.dat, rows, cols, fonttype) {
  #https://stackoverflow.com/questions/28166168/how-to-change-fontface-bold-italics-for-a-cell-in-a-kable-table-in-rmarkdown
  # select the correct markup
  map <- setNames(c("*", "**", "~~"), c("italics", "bold", "strikethrough"))
  markup <- map[fonttype]  
  
  for (r in rows){
    for(c in cols){
      
      # Make sure fonttypes are not factors
      df.dat[[c]] <- as.character( df.dat[[c]])
      
      # Update formatting
      df.dat[r, c] <- paste0(markup, df.dat[r, c], markup)
    }
  }
  
  return(df.dat)
}


########FILE ORGANIZATION#########


#' Name nth item in order (001) 
#' 
#' Convert a value. 
#' @param x The values. 
#' @keywords Data Management
#' @export
#' @examples 
#' numbers0(x = c(1, 3, 6, 101))
#' #[1] "001" "003" "006" "101"
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
#' @examples
#' auto_counter()
auto_counter<-function(counter0) {
  counter00<-ifelse(as.numeric(counter0) %in% 0, 1, as.numeric(counter0)+1)
  counter<-numbers0(c(counter00, as.numeric(paste0("1", 
                                                   paste(rep_len(x = 0, length.out = (nchar(counter0)-1)), 
                                                         collapse = "")))))[1]
  return(counter)
}


#######FILE CONTENT#######


#' Find the age of the file, when it was created. 
#' 
#' Find the age of the file, when it was created. 
#' @param path Path to the file. 
#' @keywords Data Management
#' @export
#' @examples
#' ageoffile()
ageoffile<-function(path, format = "%B %d, %Y") {
  # system("touch temp")
  info <- file.info(path)
  x<-format(info$mtime, format)
  return(x)
}


#######TABLE AND GRAPHS#######


#' Systematically save your ggplot figure for your report
#'
#' @param plot0 The ggplot you would like to be saved
#' @param plot.list The list where all plots will be saved. 
#' @param Header The name and title of the figure. Default = "".
#' @param filename0 The filename for your chapter
#' @param cnt.chapt.content The order number that this exists in the chapter
#' @param cnt.figures The figure number 
#' @param path The path the file needs to be saved to.
#' @param width Default = 6 inches
#' @param height Default = 6 inches
#'
#' @return
#' @export
#'
#' @examples
#' SaveGraphs()
SaveGraphs<-function(plot0, plot.list, Header = "", filename0, cnt.chapt.content, cnt.figures, 
                     path, width = 6, height = 6){
  ggsave( # save your plot
    path = path, 
    filename = paste0(filename0, cnt.chapt.content, "_Fig_", cnt.figures, 
                      ".pdf"), # Always save in pdf so you can make last minute edits in adobe acrobat!
    plot = plot0, # call the plot you are saving
    width = width, height = height, units = "in") #recall, A4 pages are 8.5 x 11 in - 1 in margins
  
  plot.list<-c(plot.list, plot)
  names(plot.list)[length(plot.list)]<-Header
  
  return(plot.list)
}



#' Systematically save your report tables for your report
#'
#' @param table.raw Optional. The data.frame that has no rounding and no dividing of numbers (good to save this for record keeping). Default = NA. 
#' @param table.print The data.frame as table will be seen in the report.
#' @param Header The header or title of your table
#' @param Footnotes Footnotes for the whole table. Default = NA.
#' @param filename0 The name you want to save this file as.
#' @param dir.chapters Directory where you are saving all of your chapter word documents to.
#' @param dir.tables Directory where you are saving all of your tables to. 
#' @param cnt.chapt.content The order number that this exists in the chapter
#'
#' @return
#' @export
#'
#' @examples
#' SaveTables()
SaveTables<-function(table.raw = NULL, table.print, Header, Footnotes = NA, 
                     filename0, dir.chapters, dir.tables, cnt.chapt.content){
  
  # Save raw file (no rounding, no dividing)
  if (!(is.null(table.raw))) {
    write.table(x = table.raw,  
                file = paste0(dir.tables, filename0, "_raw.csv"), 
                sep = ",",
                row.names=FALSE, col.names = F, append = F)
  }
  
  # Save file of content going into the report
  write.table(x = table.print,  
              file = paste0(dir.tables, filename00, "_print.csv"), 
              sep = ",",
              row.names=FALSE, col.names = F, append = F)
  
  write.table(x = table.print,  
              file = paste0(dir.chapters, filename00, ".csv"), 
              sep = ",",
              row.names=FALSE, col.names = F, append = F)
  
  # Save file with header and footnotes
  
  write.table(Header,  
              file = paste0(dir.tables, filename00, "_handout.csv"), 
              sep = ",",
              row.names=FALSE, col.names = F, append = F)
  
  write.table(table.print,  
              file = paste0(dir.tables, filename00, "_handout.csv"), 
              sep = ",",
              row.names=FALSE, col.names = F, append = T)
  
  if (!is.null(Footnotes) | Footnotes %in% "") {
    
    write.table("",  
                file = paste0(dir.tables, filename00, "_handout.csv"), 
                sep = ",",
                row.names=FALSE, col.names = F, append = T)
    
    a<-strsplit(x = Footnotes, split = " 123456789 ")[[1]]
    a<-unique(a)
    write.table(a,  
                file = paste0(dir.tables, filename00, "_handout.csv"), 
                sep = ",",
                row.names=FALSE, col.names = F, append = T)
  }
  
  # #footnote-ify table.print footnote column for rmarkdown
  # if (is.data.frame(table.print)) {
  #   table.print$Footnotes<-list2string.ft(x = table.print$Footnotes)
  # }
}


########METADATA########


#' Create Metadata. 
#' 
#' Create Metadata. 
#' @param dir.out Path file will be saved to.
#' @param data The data.frame.
#' @param title Title of file.
#' @keywords metadata
#' @export
#' @examples
#' CreateLoadedDataMetadata()
CreateLoadedDataMetadata<-function(dir.out, data, title) {
  my_doc <- read_docx() 
  styles_info(my_doc)
  
  my_doc <- my_doc %>% 
    body_add_par(title,
                 style = "heading 1") %>%
    
    body_add_par("Code Author", style = "heading 2") %>%
    body_add_par("Writiten by Emily Markowitz, emilyhmarkowitz@gmail.com/emily.markowitz@noaa.gov", style = "Normal") %>%  
    body_add_par("Date Code Ran:", style = "heading 2") %>%
    body_add_par(Sys.time(), style = "Normal") %>%
    # body_add_par("System Info:", style = "heading 2") %>%
    # body_add_par(paste0(Sys.info()[[1]], " ", R.version$platform), style = "Normal") %>%
    # body_add_par("R Version", style = "heading 2") %>%
    # body_add_par(paste0(R.version$version.string, ": ", R.version$nickname), style = "Normal") #%>%
    body_add_par("Input Data used in this Run",
                 style = "heading 2")
  
  for (i in 1:length(loaded.data)){
    
    temp<-loaded.data[[i]]
    
    my_doc <- my_doc %>%
      body_add_par(names(loaded.data)[i], style = "heading 3") %>%
      body_add_table(head(temp)) %>%
      body_add_par(summary(temp), style = "Normal")
  }
  
  # a<-sessionInfo()
  # my_doc <- my_doc %>% 
  #   body_add_par("R Packages Loaded", style = "heading 2")
  # for (i in 1:length(a$basePkgs)){
  #   my_doc <- my_doc %>% 
  #     body_add_par(a$basePkgs[i], style = "Normal") 
  # }
  # for (i in 1:length(a$otherPkgs)){
  #   temp<-a$otherPkgs[[i]]
  #   my_doc <- my_doc %>% 
  #     body_add_par(temp$Package,
  #                  style = "heading 3") %>%
  #     body_add_par(temp$Version, style = "Normal") %>%
  #     body_add_par(temp$Title, style = "Normal") %>%
  #     body_add_par(temp$Description, style = "Normal") %>%
  #     body_add_par(temp$SystemRequirements, style = "Normal") %>%
  #     body_add_par(paste0(temp$`Authors@R`), style = "Normal") %>%
  #     body_add_par(temp$URL, style = "Normal")
  # }
  
  print(my_doc, target = paste0(dir.out, "/Metadata_", Sys.Date(), ".docx"))
}


#' Create CreateMetadata 
#' 
#' Create Metadata. 
#' @param dir.out Path file will be saved to.
#' @param title Title of file.
#' @keywords metadata
#' @export
#' @examples
#' CreateMetadata()
CreateMetadata<-function(dir.out, title){
  my_doc <- read_docx() 
  styles_info(my_doc)
  
  my_doc <- my_doc %>% 
    body_add_par(title,
                 # body_add_par(paste0("Population Narrative of ", commorg, " (", fp_text(sciname, italic = T, color = "black", font.size=10), ")"," in ", region),
                 style = "heading 1") %>%
    body_add_par("Date Code Ran:", style = "heading 2") %>%
    body_add_par(Sys.time(), style = "Normal") %>%
    body_add_par("System Info:", style = "heading 2") %>%
    body_add_par(paste0(Sys.info()[[1]], " ", R.version$platform), style = "Normal") %>%
    body_add_par("R Version", style = "heading 2") %>%
    body_add_par(paste0(R.version$version.string, ": ", R.version$nickname), style = "Normal") #%>%
  #   body_add_par("Populations Run in this Iteration", 
  #                style = "heading 2") 
  # for (i in 1:length(org_pop)){
  #   my_doc <- my_doc %>% 
  #     body_add_par(org_pop[i], style = "Normal") 
  # }
  
  a<-sessionInfo()
  my_doc <- my_doc %>% 
    body_add_par("R Packages Loaded", style = "heading 2")
  for (i in 1:length(a$basePkgs)){
    my_doc <- my_doc %>% 
      body_add_par(a$basePkgs[i], style = "Normal") 
  }
  for (i in 1:length(a$otherPkgs)){
    temp<-a$otherPkgs[[i]]
    my_doc <- my_doc %>% 
      body_add_par(temp$Package,
                   style = "heading 3") %>%
      body_add_par(temp$Version, style = "Normal") %>%
      body_add_par(temp$Title, style = "Normal") %>%
      body_add_par(temp$Description, style = "Normal") %>%
      body_add_par(temp$SystemRequirements, style = "Normal") %>%
      body_add_par(paste0(temp$`Authors@R`), style = "Normal") %>%
      body_add_par(temp$URL, style = "Normal")
  }
  
  print(my_doc, target = paste0(dir.out, "/Metadata_", Sys.Date(), ".docx"))
}



