# -----------------------------------------------------------------------------
# Emily Mitchell
# Updated: 8/28/2023
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

options(survey.lonely.psu='adjust')

source("functions.R")

# Update file names and run this for secure LAN, to use pre-puf files
# file_names <- list(FYC = "")
# source("functions_readMEPS.R")

# Specify location of CCSR crosswalk for COND tables
# ccs_url  <- "https://raw.githubusercontent.com/HHS-AHRQ/MEPS/master/Quick_Reference_Guides/meps_ccs_conditions.csv"
 ccsr_url <- "https://raw.githubusercontent.com/HHS-AHRQ/MEPS/master/Quick_Reference_Guides/meps_ccsr_conditions.csv"

apps <- c(
  "hc_use", "hc_ins", "hc_pmed", 
  "hc_care_access", "hc_care_diab", "hc_care_qual",
  #"hc_cond_icd9",   
  "hc_cond_icd10") 


# Year (or years) that needs to be run
#
# OPTIONAL: Make a copy of 'data_tables' folder to run QC years
#  - rename to 'data_tables - orig'


  year_list = 2020:2021  # 5/10/24 - re-running PMED tables with TC1 edit
  #year_list = 2021
  hc_year <- max(year_list)

  
# Define grouping variables ---------------------------------------------------
  
  demo_grps <- c(
    "ind", "agegrps", "region", "married", "race", "sex", "education",
    "employed", "insurance", "poverty", "health", "mnhlth")
  
  pmed_grps <- c("TC1name", "RXDRGNAM")
  
  ins_grps  <- c("insurance", "ins_lt65", "ins_ge65")

  
# Create tables for new data year ---------------------------------------------

  # Create new tables for data year -- takes about 3 hours
 
  source("run_ins.R")  # ~ 4 min
  source("run_pmed.R") # ~ 2 min 
  
  source("run_care_access.R") # Shift in variables in 2018
  source("run_care_diab.R")   
  source("run_care_qual.R")   # Only odd years, starting 2017 (2002-2017, 2019, 2021,...)
  
  source("run_cond.R") # 5/22/23: updating with new groups for 2016-2020
  
  
  source("run_use.R")  # ~ 1 hr 

  
  # QC tables for new year -- need to update for hc_cond_icd10 to include more years
    log_file <- "qc/update_files/update_log.txt"
    source("qc/UPDATE_check.R")
  
  ## STOP!! CHECK LOG (qc/update_files/update_log.txt) before proceeding
  

  
# Format tables --------------------------------------------------------------
    
  # Output to formatted_tables folder
  # totPOP for 'Any event' is updated -- old version was including all people, including those with no events

  source("functions_format.R")  
  
  yrs <- 2020:2021
  
  format_tables(appKey = "hc_use",  years = yrs)  
  format_tables(appKey = "hc_ins",  years = yrs)
  
  format_tables(appKey = "hc_pmed", years = yrs)
  # PMED post-processing: 
  #  - remove any suppressed rows
  #  - remove RXDRGNAMs that were masked to therapeutic classes
  #  - for 1996-2013, remove RXDRGNAM that do not show up in 2014-2019
  source("code/postprocessing_pmed.R")
  
  
  #format_tables(appKey = "hc_cond_icd9",  years = 1996:2015)
  format_tables(appKey = "hc_cond_icd10", years = yrs)
  source("code/postprocessing_cond.R")
  
  
  format_tables(appKey = "hc_care_access", years = yrs)
  format_tables(appKey = "hc_care_diab",   years = yrs)
  format_tables(appKey = "hc_care_qual",   years = yrs)
  
  
  
  
# Prepare formatted tables for delivery to CVP for Tableau dashboards ---------
  
  year <- 2020
  
  today  <- Sys.Date()
  newdir <- str_glue("deliveries/DY{year}-{today}")
  
  dir.create(newdir, recursive = T)
  
 #apps = "hc_cond_icd10"
  for(app in apps) {
    str_glue("formatted_tables/{app}/DY{year}.csv") %>%
      file.copy(str_glue("{newdir}/{app}_{year}.csv"))
  }
 
# special delivery 2023-05-26: Updated COND tables for 2016-2020
#  - updated CCSR groupings after Spring 2022 review
#  - now using PUFs for 2016/2017 data (instead of secure LAN data)
#  - adding BODY system to COND tables


  
#   # special 2022-08-29 delivery: All PMED files 1996-2020 (since we fixed some issues)
#   pmed_files = list()
#   for(year in 1996:2020) {
#     DY = paste0("DY", year)
#     pmed_files[[DY]] = read_csv(str_glue("formatted_tables/hc_pmed/{DY}.csv"))
#   }
#   all_pmed_files = bind_rows(pmed_files)
#   write_csv(all_pmed_files, str_glue("{newdir}/hc_pmed_1996_to_2020.csv"), na = "")
#   
#   
  
    
  