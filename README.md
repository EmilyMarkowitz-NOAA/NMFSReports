# NMFSReports

Easily write NOAA reports and Tech Memos in R Markdown

> Code is still in development

**Emily Markowitz** (Emily.Markowitz AT noaa.gov)

Alaska Fisheries Science Center, 

National Marine Fisheries Service, 

National Oceanic and Atmospheric Administration,

Seattle, WA 98195


## Install this package: 

Learn more about this package at this pkgdown webpage! https://emilymarkowitz-noaa.github.io/NMFSReports/

```r
library(devtools)

# Functions for Writing R Markdown Reports 
devtools::install_github("EmilyMarkowitz-NOAA/NMFSReports")

```

## Use this package with `nmfspalette`:

```r

# nfmspalette                    

library(devtools)

# NMFS color pallette
devtools::install_github("nmfs-general-modeling-tools/nmfspalette")                    
                    
```

## Inspiration

As the previous editor of the fisheries economics of the US report (FEUS, published by the office of science and technology in Silver Spring Maryland) I’ve been thinking about how to create reproducible documents and rmarkdown for some time now. Using what I learned from producing FEUS, I started building a personal package for me to use in future report writing. It’s clear to me that reproducible reports is a massive need across NOAA, and this pet project snowballed into an opportunity to help others and I decided to make that package available to others who might also find it helpful. It’s very much in the development stage, but I think it has many of the nuts and bolts to be something that can be tested and worked on collaboratively. Anyone who’s interested in putting this to use and seeing what improvements can be made or even collaborating on making this package that more useful I welcome their interest and their insight!

This package takes you through most of the steps of writing a report. The buildTM() create the initial skeleton and architecture for the report, And the rest of the functions help you systematically and reproducibly write your report. I created a detailed package down page I’ll make it up that explains the function of each of these functions and vignettes of what to expect at each step of the process.

## NOAA README

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## NOAA License

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

