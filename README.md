# RMarkReports

**PRELIMINARY**: Write smart R Markdown reports with these basic and easy funcitons!

> Code is still in development

**Emily Markowitz** (Emily.Markowitz AT noaa.gov)

Alaska Fisheries Science Center, 

National Marine Fisheries Service, 

National Oceanic and Atmospheric Administration,

Seattle, WA 98195


## Use this package: 

```r
library(devtools)

# Functions for Writing R Markdown Reports 
devtools::install_github("EmilyMarkowitz-NOAA/RMarkReports")

```

## Use this package with `NOAATechMemoStarterKit` and `nmfspalette`:

```r
# NOAATechMemoStarterKit

install.packages("usethis")

library(usethis) # Automate package and project setup tasks that are otherwise performed manually.

usethis::use_course(url = 'https://github.com/EmilyMarkowitz-NOAA/NOAATechMemoStarterKit/archive/master.zip', 
                    destdir = "your/local/directory/")

# nfmspalette                    

library(devtools)

# NMFS color pallette
devtools::install_github("nmfs-general-modeling-tools/nmfspalette")                    
                    
```

## NOAA README

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## License

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

