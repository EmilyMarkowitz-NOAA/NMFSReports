#' ---
#' title: # INSERT_REPORT_TITLE
#' author: # INSERT_AUTHOR
#' purpose: Download data
#' start date: # YYYY-MM-DD
#' date modified: # YYYY-MM-DD        # CHANGE
#' Notes:                             # CHANGE
#' ---

# Example Data

# https://www.fisheries.noaa.gov/alaska/commercial-fishing/alaska-groundfish-bottom-trawl-survey-data

# Download EBS
# download.file(url = "https://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/ebs2017_2018.zip", 
#               destfile=paste0("./data/ebs2017_2018.zip") )
# 
# 
# zip::unzip(zipfile = paste0("./data/ebs2017_2018.zip"), 
#              overwrite = T,
#              exdir = paste0("./data/"))
