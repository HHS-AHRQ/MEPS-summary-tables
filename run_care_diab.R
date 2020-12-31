# Create directory ------------------------------------------------------------

tbl_dir <- "data_tables/hc_care_diab"
dir.create(tbl_dir)

# Define groups ---------------------------------------------------------------
row_grps <- demo_grps %>% add_v2X

# Run for specified year(s) ---------------------------------------------------
for(year in year_list[year_list >= 2002]) {
  
  dir.create(sprintf('%s/%s', tbl_dir, year))
  
  yr <- substring(year, 3, 4)
  
# Load files, merge, create subgroups, and svydesigns -----------------------
  
  source("code/load_fyc.R", echo = T)     # Load FYC file
  source("code/add_subgrps.R", echo = T)  # Define subgroups
  source("code/add_caregrps.R", echo = T) # Add CARE subgroups
  
  source("code/dsgn_care.R", echo = T)    # Define all survey design objects
  
  
# Loop over row_grps (demographic vars) and stats -----------------------------
  
  stat_FUNS <- list(
    totPOP = svytotal, 
    pctPOP = svymean, 
    n = unwtd.count)
  
  for(row in row_grps) { print(row)
    for(stat in names(stat_FUNS)) { print(stat)
      
      by_form <- as.formula(sprintf("~%s", row))
      func <- stat_FUNS[[stat]]
      
      res <- list()
      
      res[["diab_a1c"]]  = svyby(~diab_a1c,  FUN = func, by = by_form, design = DIABdsgn)
      res[["diab_eye"]]  = svyby(~diab_eye,  FUN = func, by = by_form, design = DIABdsgn)
      res[["diab_flu"]]  = svyby(~diab_flu,  FUN = func, by = by_form, design = DIABdsgn)
      res[["diab_chol"]] = svyby(~diab_chol, FUN = func, by = by_form, design = DIABdsgn)
      res[["diab_foot"]] = svyby(~diab_foot, FUN = func, by = by_form, design = DIABdsgn)
      
      # Format and output to csv ----------------------------------------------
      stat_se = p(stat, "_se")
      
      for(rs in names(res)) {
     
        if(stat == "n") {
          out <- res[[rs]] %>% setNames(c("rowLevels", stat, stat_se))
          
        } else { # re-group for totPOP and pctPOP
          out <- res[[rs]] %>% 
            stdize(row = row, stat = stat) %>%
            mutate(colLevels = sub(rs,"",colLevels))
        }
        
        out %>% mutate(rowGrp = row, colGrp = rs) %>%
          update.csv(file = sprintf("%s/%s.csv", year, stat), dir = tbl_dir)
      }

    } # END for stat in names(stat_FUNS)
  } # END for row in row_grps
  
} # END for year in year_list