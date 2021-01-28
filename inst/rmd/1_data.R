#' ---
#' title: Data Report: Load Data
#' purpose: Load data from survey for data report
#' author: emily.markowitz AT noaa.gov
#' start date: 2020-10
#' ---

#Example Data

Footnotes.list<-list("ExOfStandardFt" = "Wow, this project is so cool!")

SURVEY<-"eastern Bering Sea"
sectname<-"EBS-BTS-Report"

dat<-read.csv(file = "./data/ebs2017_2018.csv")

write.csv(x = dat, file = paste0(dir.rawdata, "/ebs2017_2018.csv"))
