#' ---
#' title: Report Template: Download Data
#' purpose: Download relevant data
#' author: Me, myself, and I (me.noaa.gov)
#' start date: YYYY-MM
#' Notes: This is cool data... 
#' ---

# Example Data

# https://www.fisheries.noaa.gov/alaska/commercial-fishing/alaska-groundfish-bottom-trawl-survey-data

# Download EBS
download.file(url = "https://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/ebs2017_2018.zip", 
              destfile=paste0("./data/ebs2017_2018.zip") )


zip::unzip(zipfile = paste0("./data/ebs2017_2018.zip"), 
             overwrite = T,
             exdir = paste0("./data/"))
