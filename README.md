
# NMFSReports

<!-- badges: start -->
<!-- badges: end -->

Easily write NOAA reports and Tech Memos in R Markdown

> Code is still in development. 

**Emily Markowitz** (Emily.Markowitz AT noaa.gov)

Alaska Fisheries Science Center, 

National Marine Fisheries Service, 

National Oceanic and Atmospheric Administration,

Seattle, WA 98195

## Installation

Learn more about this package at this pkgdown webpage! https://emilymarkowitz-noaa.github.io/NMFSReports/

The NMFSReports Package has all of the basic architecture you need to create reproducible and repeatable NOAA Tech Memos in R Markdown! This approach is perfect for efficiently rolling out annual (or other regular) reports or reports with formulaic sections (the same chapter structure but for a different area or species). Scripts integrate table, figure, data, and bibliography management and design automation.


```r
library(devtools)
devtools::install_github("EmilyMarkowitz-NOAA/NMFSReports")
library(NMFSReports)

# Or, alternatively, 
remotes::install_github("EmilyMarkowitz-NOAA/NMFSReports@main")
```

#### Use this package with `nmfspalette`

A package for NOAA Fisheries color schemes

```r
library(devtools)
devtools::install_github("nmfs-general-modeling-tools/nmfspalette")
library(nmfspalette)
```

## Inspiration

I’ve been thinking about how to efficiently create reproducible documents and rmarkdown scripts for some time. As the former editor of the Fisheries Economics of the US Report (FEUS, published by the NMFS Office of Science and Technology in Silver Spring, Maryland), I completely altered the workflow from a report that was edited line by line to a report that was created reproducibly by a click of a button (Check out this DC satRdays 2020 presentation (March 28 2020): https://www.youtube.com/watch?v=-mycRwaC60A for more info). Using what I learned from re-coding the FEUS and other reports since then, I started building a personal package that I could use in future report writing. It is clear to me that reproducible reports are a massive need across NOAA, and this pet project became an opportunity to help others. Though it is very much in the development stage, but I think it is ready to be tested and worked on collaboratively. I welcome any potential collaboration or comments on how to improve this package.

This package takes you through most of the steps of writing a report. The buildReport() functiton creates the initial skeleton and architecture for the report, and the rest of the functions help you systematically and reproducibly write your report. 


## NOAA README

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## NOAA License

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.



