# Commentary to twinR 

This is the repository providing the code and the data associated with the commentary on the paper
"[**Mothers with higher twinning propensity had lower fertility in pre-industrial Europe**](https://doi.org/10.1038/s41467-022-30366-9)" (Nature Communications, 2022) 


## Running Code

Running code from this package expects that you have R 4.1.0 or later installed. 

First, make sure the [twinR package](https://github.com/courtiol/twinR) and its dependencies are installed. First install the package **{remotes}** (if it is not already installed on your system), then simply type the following in your R Console:

```r
remotes::install_github("courtiol/twinR", dependencies = TRUE)
```

Then, you can clone this repository and run the code from the Rmarkdown document "S1_replicate_stats_and_plots.Rmd" line by line using RStudio to reproduce the analyses. If you wish to convert this into R script to run from console you can e.g. use:

```r
knitr::purl("S1_replicate_stats_and_plots.Rmd", output="S1_replicate_stats_and_plots.R")
```
## Changes from twinR Package Functions

This repository has made few small fixes to 2 functions in twinR package in order to be run on different environments:

* pipe operator (%>%) from tidyverse has been replace by base R pipe operator (|>) 
* cAIC calculation has been omitted from model summary table
* fit summary table has been refactored to run seamlessly with 0 lambdas

Also a function has been added to extract the last birth for each mother as well as some other convenience functions.

