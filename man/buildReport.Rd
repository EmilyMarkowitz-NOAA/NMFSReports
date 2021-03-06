% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NMFSReports.R
\name{buildReport}
\alias{buildReport}
\title{Build your intitial architecture for your new NOAA Tech Memo or Report}
\usage{
buildReport(
  sections = c("abstract", "introduction", "methods", "results", "discussion",
    "endmatter", "presentation"),
  report_authors = "",
  report_title = "",
  styles_reference_pptx = "refppt_nmfs",
  styles_reference_docx = "refdoc_noaa_tech_memo",
  bibliography.bib = "bib_example",
  csl = "bulletin-of-marine-science"
)
}
\arguments{
\item{sections}{a string of the different sections of your report. Sections must be listed in order. Default = c("frontmatter", "abstract", "introduction", "methods", "results", "discussion", "endmatter"). Note that "frontmatter" and "endmatter" both have specific templates, and all others are from a blank template. "endmatter" will document all of your citations throughout the report, the R packages you used to create this report. I'm biased, but please give credit where credit is due! There are also spots here to list authors's ORCID and acknowlegelments.}

\item{report_authors}{Default = "". Here, add all author's first and last name as it should appear in the report.You can change this later by editing this in the run.R file.}

\item{report_title}{Default = "". Here, put the title of your report. You can change this later by editing this in the run.R file.}

\item{styles_reference_pptx}{A style reference guide from a powerpoint document (.pptx). This pulls the styles from a powerpoint document where you have defined each style. Either use NULL to not have a presentation, a local document (insert full path to local document), or a pre-made templates ("refppt_nmfs"). Default = "refppt_nmfs". You can change this later by renaming the file in the code folder.}

\item{styles_reference_docx}{A style reference guide from a word document (.docx). This pulls the styles from a word document where you have defined each style. Either use a local document (insert full path to local document) or some of the pre-made templates ("refdoc_noaa_tech_memo" or "refdoc_fisheries_economics_of_the_us"). Default = "refdoc_noaa_tech_memo". You can change this later by renaming the file in the code folder.}

\item{bibliography.bib}{Either use a local document (.bib format; insert full "path") or the example file from the package ("bib_example"). Default = "bib_example". You can change this later by renaming the file in the cite folder.}

\item{csl}{Citation style. Either use a local document (insert full path to local document) or some of the pre-made templates ("bulletin-of-marine-science"). A NOAA TM citation style needs to be created, but until then, the default = "bulletin-of-marine-science". You can change this later by renaming the file in the cite folder. Find citation styles at: https://github.com/citation-style-language/styles}
}
\value{
complete initial architecture for your R Markdown Report.
}
\description{
Build your intitial architecture for your new NOAA Tech Memo or Report
}
\examples{
sections = c("frontmatter", "abstract", "introduction", "methods", "results",
            "discussion", "endmatter")
authors = "Me, Myself, and I"
title = "Awesome Report!"
styles_reference_pptx = "refppt_nmfs"
styles_reference_docx = "refdoc_noaa_tech_memo"
bibliography.bib = "bib_example"
csl = "bulletin-of-marine-science"

# not run:
# buildReport(
#   sections = sections,
#   report_authors = authors,
#   report_title = title,
#   styles_reference_pptx = styles_reference_pptx,
#   styles_reference_docx = styles_reference_docx,
#   bibliography.bib = bibliography.bib,
#   csl = csl
# )
}
