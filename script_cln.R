# SETUP ------

## (TBD) Local Env ------
loc  <- "G://00_PhD_II//Rfolder//bulkdwEC//" #getwd()
loca <- "G://00_PhD_II//Rfolder//bulkdwEC//auxi//"
locd <- "G://00_PhD_II//Rfolder//bulkdwEC//data//"
loct <- "G://00_PhD_II//Rfolder//bulkdwEC//temp//"


## Libraries ------
          # install.packages(
          # c("tidyverse", "lubridate", "eurostat", 
          # "knitr", 
          # "ggfortify", "changepoint", "strucchange", "ggpmisc")
          # )

library (tidyverse, # Data Management v1.3.0*
         quietly=T, warn.conflicts=F, logical.return=F)
library (lubridate, # Dates management V1.7.8
         quietly=T, warn.conflicts=F, logical.return=F)
library (eurostat, # Retrieve time series from Eurostat
         quietly=T, warn.conflicts=F, logical.return=F)
library (knitr, # Table formatting v1.28
         quietly=T, warn.conflicts=F, logical.return=F)
library (ggfortify, # Time Series analysis v0.4.10
         quietly=T, warn.conflicts=F, logical.return=F)
library (changepoint, # Time Series analysis v2.2.2
         quietly=T, warn.conflicts=F, logical.return=F)
library (strucchange, # TS Change points v1.5.2
         quietly=T, warn.conflicts=F, logical.return=F)
library (ggpmisc, # TS picks and valleys v0.3.5
         quietly=T, warn.conflicts=F, logical.return=F)

          # *tidyverse 1.3.0 includes:
          # v ggplot2 3.3.0     v purrr   0.3.4
          # v tibble  3.0.1     v dplyr   0.8.5
          # v tidyr   1.1.0     v stringr 1.4.0
          # v readr   1.3.1     v forcats 0.5.0



## Function variables -------
  #First part of the REST request pattern to a data file
  fdatcon <- "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=data%2F"

  #Second part of the REST request pattern for TSV files
  fdatpat <- ".tsv.gz"
  
  
## Functions ------
  #Generate transformed connection to compressed files
  contr <- function (code) {
      tcon <- gzcon ( 
        url ( 
          paste0 (
            fdatcon, code, fdatpat)
        )
      )
      return (tcon)
  }
  
  