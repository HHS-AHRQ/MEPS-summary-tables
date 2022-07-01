# -----------------------------------------------------------------------------
# Emily Mitchell
# Updated: 12/22/2020
# 
# MEPS tables creation
#  - Run this code to update MEPS tables for new data years
# -----------------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(survey)
library(MEPS)
library(readxl)
library(htmltools)
library(haven) 

source("functions.R")

apps <- c(
  "hc_use", "hc_ins", "hc_pmed", 
  "hc_care_access", "hc_care_diab", "hc_care_qual",
  #"hc_cond_icd9",   
  "hc_cond_icd10") 


# Year (or years) that needs to be run
# OPTIONAL: Make a copy of 'data_tables' folder to run QC years
#  - rename to 'data_tables - orig'

  #year_list <- c(2014, 2018)
  year_list = 2019
  hc_year <- max(year_list)

  
# Define grouping variables ---------------------------------------------------
  
  demo_grps <- c(
    "ind", "agegrps", "region", "married", "race", "sex", "education",
    "employed", "insurance", "poverty", "health", "mnhlth")
  
  pmed_grps <- c("TC1name", "RXDRGNAM")
  
  ins_grps  <- c("insurance", "ins_lt65", "ins_ge65")

  
# Create tables for new data year ---------------------------------------------

  ## !! For hc_cond icd10 versions (2016, 2017), need to build tables on secure
  ## !! LAN, since CCSR codes are not on PUFs 

  # Create new tables for data year -- takes about 3 hours
 
  source("run_ins.R")  # ~ 4 min
  source("run_pmed.R") # ~ 2 min 
  
  source("run_care_access.R") # Shift in variables in 2018
  source("run_care_diab.R")   
  source("run_care_qual.R")   # Only odd years, starting 2017 (2002-2017, 2019, 2021,...)
  
  source("run_cond.R") # do NOT run for 2016/2017 -- only available on Secure LAN
  source("run_use.R")  # ~ 1 hr 

  
  # QC tables for new year -- need to update for hc_cond_icd10 to include more years
    log_file <- "qc/update_files/update_log.txt"
    source("qc/UPDATE_check.R")
  
  ## STOP!! CHECK LOG (qc/update_files/update_log.txt) before proceeding
  
  ## Transfer 2016/2017 hc_cond_icd10 tables here before formatting
    
  
# Format tables --------------------------------------------------------------
    
  # Output to formatted_tables folder
  # totPOP for 'Any event' is updated -- old version was including all people, including those with no events

  source("functions_format.R")  
  
  yrs <- 2019
    
  format_tables(appKey = "hc_ins",  years = yrs)
  format_tables(appKey = "hc_pmed", years = yrs)
  format_tables(appKey = "hc_use",  years = yrs)
  
  #format_tables(appKey = "hc_cond_icd9",  years = 1996:2015)
  format_tables(appKey = "hc_cond_icd10", years = yrs)
  
  format_tables(appKey = "hc_care_access", years = yrs)
  format_tables(appKey = "hc_care_diab",   years = yrs)
  format_tables(appKey = "hc_care_qual",   years = yrs)
  
  
# Prepare formatted tables for delivery to CVP for Tableau dashboards ---------
  
  year <- 2019
  
  today  <- Sys.Date()
  newdir <- str_glue("deliveries/DY{year}-{today}")
  
  dir.create(newdir, recursive = T)
  
  for(app in apps) {
    str_glue("formatted_tables/{app}/DY{year}.csv") %>%
      file.copy(str_glue("{newdir}/{app}_{year}.csv"))
  }
 

  
  
  
  
  
  
  
  
  
  
  