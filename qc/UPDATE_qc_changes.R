# -----------------------------------------------------------------------------
# Emily Mitchell
# Updated: 12/23/2020
# 
# Compare original vs new data files after code re-factor or major changes
# -----------------------------------------------------------------------------

library(testthat)
library(tidyverse)

source("functions.R")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

apps <- c(
  "hc_care_access", 
  "hc_care_diab",
  "hc_care_qual",
  "hc_cond_icd9", 
  "hc_cond_icd10", 
  "hc_ins", 
  "hc_pmed", 
  "hc_use")


# app  <- "hc_cond_icd9"

year <- 2014; chk_apps <- apps %>% pop("hc_cond_icd10");
year <- 2018; chk_apps <- apps %>% pop("hc_cond_icd9");


for(app in chk_apps) { cat("\n\n", app)
  
  yr <- substr(year, 3, 4)
  
  orig_folder <- str_glue("data_tables - orig/{app}/{year}")
  new_folder  <- str_glue("data_tables/{app}/{year}")
  
  orig_csvs <- list.files(orig_folder)
  new_csvs  <- list.files(new_folder)
  
  compare(orig_csvs, new_csvs) 
  
  
  for(file in orig_csvs) { cat("\n...",file)
    
    stat = file %>% gsub(".csv","", ., fixed = T)
    
    orig_file <- orig_dup <- read.csv(str_glue("{orig_folder}/{file}"))
    new_file  <- new_dup  <- read.csv(str_glue("{new_folder}/{file}"))
    
    if(app == "hc_use") {
      orig_dup <- bind_rows(orig_file, switch_labels(orig_file))
      new_dup  <- bind_rows(new_file, switch_labels(new_file))
      
      # Remove 'Missings' 
      new_dup  <- new_dup  %>% filter(colLevels != "Missing", rowLevels != "Missing")
      orig_dup <- orig_dup %>% filter(colLevels != "Missing", rowLevels != "Missing")
      
      # Remove non-physician events
      new_dup <- new_dup %>%
        filter(!colLevels %in% c("OBO", "OPZ"), !rowLevels %in% c("OBO", "OPZ"))
      orig_dup <- orig_dup %>%
        filter(!colLevels %in% c("OBO", "OPZ"), !rowLevels %in% c("OBO", "OPZ")) 
      
      # Remove SEs for 'n' files
      if(file == "n.csv") {
        new_dup  <- new_dup %>% select(-n_se) 
        orig_dup <- orig_dup %>% select(-n_se) 
      }
      
      
      # Median SEs are off due to R updates -- need to apply BRR to these
      if(file == "medEXP.csv") {
        new_dup  <- new_dup %>% select(-medEXP_se)
        orig_dup <- orig_dup %>% select(-medEXP_se)
      }
      
    }
    
    if(app == "hc_cond_icd9") {
      # Round vars for big numbers
      orig_dup$stat = orig_dup[,stat]
      orig_dup$se   = orig_dup[,paste0(stat,"_se")]
      
      new_dup$stat = new_dup[,stat]
      new_dup$se   = new_dup[,paste0(stat,"_se")]
      
      orig_dup <- orig_dup %>% select(rowGrp, colGrp, rowLevels, colLevels, stat, se)
      new_dup  <- new_dup  %>% select(rowGrp, colGrp, rowLevels, colLevels, stat, se)
      
      orig_dup <- orig_dup %>% mutate(stat = round(stat, 4), se = round(se, 4))
      new_dup  <- new_dup %>% mutate(stat = round(stat, 4), se = round(se, 4))
    }
    
    
    same <- all_equal(orig_dup, new_dup) 
    
    if(same != TRUE) {
      cat('\n')
      print('In ORIG only')
      diff1 <- setdiff(orig_dup, new_dup); 
      diff1 %>% head %>% print;
      #diff1 %>% count(rowGrp, colGrp) %>% print
      
      print('In NEW only')
      diff2 <- setdiff(new_dup, orig_dup); 
      diff2 %>% head %>% print;
      #diff2 %>% count(rowGrp, colGrp) %>% print
      
      #diff1 %>% as_tibble() %>% count(rowGrp, colGrp) %>% print(n = 100)
      #diff2 %>% as_tibble() %>% count(colGrp, rowGrp) %>% print(n = 100)
    } else {
      cat("...ALL SAME!! YAY!!!")
    }
    
  } # end for file in files
  
  
} # end for app in apps





# chk <- full_join(orig_dup, new_dup, 
#                  by = c("rowGrp", "colGrp", "rowLevels", "colLevels"))
# 
# chk %>% filter(colLevels == "Separated", rowGrp == "event")
# chk %>% filter(round(medEXP_se.x,1) != round(medEXP_se.y,1)) #%>% head(20)
# chk %>% filter(medEXP.x != medEXP.y) %>% head(20) # 0 rows
# chk %>% filter(is.na(medEXP.x) | is.na(medEXP.y))
# 
# orig_dup %>% filter(rowGrp == "ind", colGrp == "ind")
# new_dup %>% filter(rowGrp == "ind", colGrp == "ind")


  
# 
# # Formatted files ----------------------------------------------------------
# setwd("C:/Users/emily.mitchell/Desktop/GitHub/hhs_ahrq/MEPS-tables/build_hc_tables")
# 
# year <- 2017
# app <- "hc_use"
# 
# # The only difference should be totPOP-'Any Event' for hc_use
# # previously, was accidentally counting all people, not just those with an event
# 
# 
# for(app in apps) { cat("\n", app,"...\n")
#   
#   for(year in c(1996:2017)) { print(year)
#     
#     if(app == "hc_care" & year < 2002) next
#     if(app == "hc_cond" & year > 2015) next
#     if(app == "hc_cond_icd10" & year < 2016) next
#     
#     rm(orig_file)
#     rm(new_file)
#     
#     orig_file <- read.csv(str_glue("../formatted_tables/_archive/{app} - Copy/DY{year}.csv"))
#     new_file  <- read.csv(str_glue("../formatted_tables/{app}/DY{year}.csv"))
#     
#     byvars <- c("stat_group", "stat_var", "stat_label", "caption",
#                 "row_group", "row_var", "row_label", "rowLevels", 
#                 "col_group", "col_var", "col_label", "colLevels")
#     
#     jvars <- byvars[byvars %in% colnames(orig_file)]
#     
#     chk <- full_join(orig_file, new_file, by = jvars)
#     
#     orig_miss <- chk %>% filter(is.na(coef.x), !is.na(coef.y)) 
#     new_miss  <- chk %>% filter(is.na(coef.y), !is.na(coef.x)) 
#     new_diff  <- chk %>% filter(coef.x != coef.y | se.x != se.y)
#     
#     if(nrow(orig_miss) > 0) {
#       print(orig_miss %>% head)
#     } 
#     if(nrow(new_miss) > 0) {
#       print(new_miss %>% head)
#     } 
#     if(nrow(new_diff) > 0) {
#       print(new_diff %>% count(stat_var, colLevels) )
#     } 
#     # 
#     # orig_miss %>% head %>% print
#     # new_miss  %>% head %>% print
#     # new_diff  %>% head %>% print
#     
#     # new_miss %>% count(stat_var) 
#     # new_miss %>% count(stat_var, row_var, col_var) %>% print(n = 100)
#     # View(new_miss)
#     # 
#     # new_diff_coef %>% count(stat_var, colLevels)
#     # new_diff_se %>% count(stat_var)
#     # View(new_diff)
#     
#     
#   }
# }


# OLD (file editing) ----------------------------------------------------------

# Remove '_DONE.Rdata' files
# all_files <- list.files("data_tables - orig", recursive = T, full.names = T)
# rdatas <- all_files[all_files %>% endsWith(".Rdata")]
# 
# for(file in rdatas) {
#   unlink(file)
# }

# 
# # split original hc_care into separate groups
# 
# n_orig %>% count(colGrp)
# 
# new_access <- read.csv(str_glue("data_tables/hc_care_access/2014/n.csv"))
# new_diab   <- read.csv(str_glue("data_tables/hc_care_diab/2014/n.csv"))
# new_qual   <- read.csv(str_glue("data_tables/hc_care_qual/2014/n.csv"))
# 
# access_groups <- new_access %>% count(colGrp) %>% pull(colGrp)
# diab_groups   <- new_diab %>% count(colGrp) %>% pull(colGrp)
# qual_groups   <- new_qual %>% count(colGrp) %>% pull(colGrp)
# 
# for(year in 2002:2018) {
#   
#   for(stat in c("n", "pctPOP", "totPOP")) {
#     
#     orig_file   <- read.csv(str_glue("data_tables - orig/hc_care/{year}/{stat}.csv"))
#     access_file <- orig_file %>% filter(colGrp %in% access_groups)
#     diab_file   <- orig_file %>% filter(colGrp %in% diab_groups)
#     qual_file   <- orig_file %>% filter(colGrp %in% qual_groups)
#     
#     leftovers <- orig_file %>% filter(!colGrp %in% c(access_groups, diab_groups, qual_groups))
#     leftovers %>% count(colGrp) %>% print
#     
#     dir.create(str_glue("data_tables - orig/hc_care_access/{year}"), showWarnings = F)
#     dir.create(str_glue("data_tables - orig/hc_care_diab/{year}"), showWarnings = F)
#     dir.create(str_glue("data_tables - orig/hc_care_qual/{year}"), showWarnings = F)
#     
#     write_csv(access_file, str_glue("data_tables - orig/hc_care_access/{year}/{stat}.csv"))
#     write_csv(diab_file,   str_glue("data_tables - orig/hc_care_diab/{year}/{stat}.csv"))
#     write_csv(qual_file,   str_glue("data_tables - orig/hc_care_qual/{year}/{stat}.csv"))
#     
#   }
# }
# # 
# # edit colLevels for SOPs to match newer code
# cond_stats <- list.files("data_tables - orig/hc_cond_icd9/1996/")
# cond_stats <- cond_stats %>% gsub(".csv","",.)
# 
# for(year in 1996:2015) { print(year)
#   yr <- substr(year, 3, 4)
# 
#   for(stat in cond_stats) { print(stat)
#     orig_file <- read.csv(str_glue("data_tables - orig/hc_cond_icd9/{year}/{stat}.csv"))
# 
#     edit_file <-  orig_file %>%
#       mutate(
#         colX = gsub(yr, "", colLevels),
#         colX = ifelse(colGrp == "sop", paste0(colX,"X"), colX),
#         colX = ifelse(colGrp == "sop", gsub("XX","X", colX), colX),
#         colLevels = ifelse(colGrp == "sop",colX, colLevels)) %>%
#       select(-colX)
# 
#     # orig_counts <- orig_file %>% count(colGrp, colLevels)
#     # edit_counts <- edit_file %>% count(colGrp, colLevels)
#     #
#     # setdiff(orig_counts, edit_counts) %>% print()
#     # setdiff(edit_counts, orig_counts)%>% print()
# 
#     write_csv(edit_file, str_glue("data_tables - orig/hc_cond_icd9/{year}/{stat}.csv"))
# 
#   }
# }
# # 
# for(year in 1996:2018) { print(year)
#   yr <- substr(year, 3, 4)
# 
#   for(stat in c("meanEVT", "totEVT")) { print(stat)
#     orig_file <- read.csv(str_glue("data_tables - orig/hc_use/{year}/{stat}.csv"))
# 
#     edit_file <-  orig_file %>%
#       mutate(
#         rowX = gsub(yr, "", rowLevels),
#         rowX = ifelse(rowGrp == "sop", paste0(rowX,"X"), rowX),
#         rowX = ifelse(rowGrp == "sop", gsub("XX","X", rowX), rowX),
#         rowLevels = ifelse(rowGrp == "sop",rowX, rowLevels)) %>%
#       select(-rowX)
# 
#     # orig_counts <- orig_file %>% count(rowGrp, rowLevels)
#     # edit_counts <- edit_file %>% count(rowGrp, rowLevels)
#     # 
#     # setdiff(orig_counts, edit_counts) %>% print()
#     # setdiff(edit_counts, orig_counts)%>% print()
# 
#    write_csv(edit_file, str_glue("data_tables - orig/hc_use/{year}/{stat}.csv"))
# 
#   }
# 
# }
# 
# De-duplicate rows in hc_pmed
# pmed_stats <- list.files("data_tables - orig/hc_pmed/1996/")
# for(year in 1996:2018) { print(year)
#   
#   for(stat in pmed_stats) { print(stat)
#     orig_file <- read.csv(str_glue("data_tables - orig/hc_pmed/{year}/{stat}"))
#     
#     edit_file <- orig_file %>% distinct
#     
#     print(nrow(orig_file))
#     print(nrow(edit_file))
#     
#     write_csv(edit_file, str_glue("data_tables - orig/hc_pmed/{year}/{stat}"))
#   }
# }