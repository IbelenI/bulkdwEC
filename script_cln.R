#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                                          
#    ##   ####   #####   ##      #####    ###   ##   ##    
#    ##   ## ##  ##      ##      ##       ## #  ##   ##      
#    ##   ####   ###     ##      ###      ##  # ##   ##    
#    ##   ## ##  ##      ##      ##       ##   ###   ##    
#    ##   ####   #####   #####   #####    ##   ###   ##    
#                                                          
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author: Belén García-Cárceles                            
# https://ibeleni.com                                      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# SETUP ------

## Local path ------
  #If necessary setup your local path
  loca <- paste0(getwd(), "//auxi//")



## Libraries and their versions ------
  library (tidyverse, # Data Management v1.3.0*
           quietly=T, warn.conflicts=F, logical.return=F)
  library (lubridate, # Dates management V1.7.8
           quietly=T, warn.conflicts=F, logical.return=F)
  library (eurostat, # Retrieve time series from Eurostat
           quietly=T, warn.conflicts=F, logical.return=F)
  library (knitr, # Table formatting v1.28
           quietly=T, warn.conflicts=F, logical.return=F)

          # *tidyverse 1.3.0 includes:
          # v ggplot2 3.3.0     v purrr   0.3.4
          # v tibble  3.0.1     v dplyr   0.8.5
          # v tidyr   1.1.0     v stringr 1.4.0
          # v readr   1.3.1     v forcats 0.5.0



  
## Connections: REST requests builders ---------
  # We are going to need
  # REST requests to connect
  # with:
  
  # The table of contents:
  con_toc <- "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=table_of_contents_en.txt"
  
  # Data directory:
  con_dir <- "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&"
  
  # Connections to data file are 
  # built using the methods
  # con_tsv or contr_tsv (see ## Methods)
  

## Methods ------

  ## Methods: Data file connections ------ 
  
  # Create string with REST request
  # to a TSV data file.
  
    # Input is a string with the 
    # id of the data file "tipsgo10".
    # Output is a string.
    con_tsv <- function (datid) {
      contsv <- paste0 (con_dir,"file=data%2F",
              datid ,".tsv.gz")
      return (contsv)
    }
  
  
  # Create string with 
  # transformed REST request
  # to a TSV data file.
    
    # Input is a string with the 
    # id of the data file "tipsgo10".
    # Output is a string.
    # Dependencies: con_tsv()
      contr_tsv <- function (datid) {
        contsvt <- gzcon ( 
          url (con_tsv(datid))
          )
          return (contsvt)
      }

      
  # Create string with
  # REST request
  # to a SDMX data file. 
      
    # Input is a string with the 
    # id of the data file "tipsgo10".
    # Output is a string.
    con_dmx <- function (datid) {
      condmx <- paste0 (con_dir,"file=data%2F",
                          datid ,".sdmx.zip")
      return (condmx)
    }

  # Create string with 
  # transformed REST request
  # to a TSV data file.
      
    # Input is a string with the 
    # id of the data file "tipsgo10".
    # Output is a string.
    # Dependencies: con_dmx()
    contr_dmx <- function (datid) {
      condmxt <- gzcon ( 
        url (con_dmx(datid))
      )
      return (condmxt)
    }
      
  ## Methods: Arrange fetched TSV files ------       

    eu_tsv <- function (x) {
      
      # Little program to unwrap the
      # first column, indicate number 
      # of NAs, first and last year, 
      # and number of observations in
      # the time series.
      
      # The input is an DATA FRAME
      # from Eurostat for annual data.
      
      # First and last observation 
      # are in the TOC data set, 
      # although I prefer to take 
      # them from the actual data.
      
      
      #--- 1- Set of functions to ### 
      # find first and last observation, 
      # NAs data between those, 
      # number of observations and
      # remove labels from observations.
      
      # Find last available observation
      findlast <- function (y){
        str_sub (names (y)[-1][which (
          !is.na (y[-1])
        )][1], 2, 5)
      }
      
      # Find first available observation
      findfirst <- function (y){
        str_sub (names (y[-1])[which (
          !is.na (y[-1]))][length (names (
            y[-1])[which (!is.na (y[-1]))]
          ) ], 2, 5)
      }
      
      # Number of NANs
      findnas <- function (y){
        length (which (is.na(y)))
      }
      
      # Number of valid observations
      numobs <- function (y){
        length (which (!is.na(y)))
      }
      
      # Remove labels
      laboff <- function (y){
        as.numeric (gsub("[^c(-inf)-9\\.]",
                         "", y))
      }
      
      
      #--- 2- Apply previous functions ###
      # to obtain the resulting data frame.
      f <- data.frame (apply (x[,-1], 2, laboff))
      
      e <- data.frame (
        end = as.integer (apply (x,1,findfirst)), 
        start = as.integer (apply (x,1,findlast)), 
        #According to documentation 
        # series are stored in descending order. 
        NAs = apply (x,1,findnas), 
        obs = apply (x, 1, numobs)
      )
      
      
      
      #--- 3- Split first column ###
      
      # First column in to char.
      char <- c (as.character (x[,1])) 
      
      # Split string in to list.
      l <- strsplit ( char, ",")
      
      # Pass it into a data frame
      d <- data.frame (
        matrix ( unlist(l), 
                 nrow=length(l), byrow=T)
      ) 
      
      
      # Name new variables in the data set:
      # Character vector with names.
      namd <- unlist (strsplit (
        str_replace_all (
          names (x)[1], "\\.",","),
        ",")) 
      
      # Assign names, discard empty var.
      names (d) <- namd[-length(namd)] 
      
      
      
      #--- 4- Devise output and return ###
      d <- cbind (e, d, f)
      return (d)
    }

    
    
         
## Using the methods ------
    # Dependencies:
    # Methods: eu_txv, con_tsv, contr_tsv
    # Connection strings: con_toc, con_dir
      
    
   ## Fetch English TOC ------
    # TOC in English
    toc <- read.table (
      con_toc, header = T, sep = "\t")
    toc$title <- str_trim (toc$title) 
      
    # Filter for tables in the TOC
    # Find "EDP" in table's titles
    ftab <- toc %>%
      filter (type=="table")
      
    #Data tables that match the case
    # Title contains "EDP":
    # Fetch the first one "tipsgo10":
    code <- ftab [grep ("EDP", 
                        ftab$title),
                  "code"][1] 
    
    # Use con_tsv () to build
    # REST request url.
    con_file <- contr_tsv ( code )
    
    #Read as text file.
    tempf <- readLines(con_file)
    
    # Pass it into a data frame, 
    # remember is a TSV file with a header.
    tipsgo10 <- read.table(
      textConnection(tempf), 
      encoding = "UTF-8", 
      sep = "\t", header = T)
    
    # Formatting
    eu_gd <- eu_tsv (tipsgo10) 

    # Reshape and filter:
    leu_gd <- eu_gd %>%
      pivot_longer (              #long format 
        c(X1999:X2019), 
        names_to = "time", 
        values_to = "values")%>%
      mutate (time =              #time numeric
                gsub ("X","", time))%>%
      filter (time>2001)%>%       #select period
      filter (geo%in%c("ES","BE"))%>%
      filter (unit=="PC_GDP") %>%
      mutate (time = factor(time)) %>%
      select (geo, values, time)  #select geo
    
    
    # Basic line plot
    theme_set(theme_minimal())
    p   <- ggplot(
      data = leu_gd, 
      aes(x = time, 
          y = values, 
          group=geo,color=geo)
    )+
      geom_line(size = 2)  
      
   p  
      