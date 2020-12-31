# Create directory ------------------------------------------------------------

tbl_dir <- "data_tables/hc_care_access"
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
      
      # Usual source of care
      res[["usc"]] = 
        svyby(~usc, FUN = func, by = by_form, 
              design = subset(FYCdsgn, ACCELI42==1 & HAVEUS42 >= 0 & LOCATN42 >= -1))
    
      # For 2002-2017, difficulty receiving needed care and reasons for difficulty
      if(year <= 2017) {
        
        res[["difficulty"]] = 
          svyby(~delay_ANY + delay_MD + delay_DN + delay_PM, 
                FUN = func, by = by_form, design = subset(FYCdsgn, ACCELI42==1))
        
        res[["rsn_ANY"]] = 
          svyby(~afford_ANY + insure_ANY + other_ANY, 
                FUN = func, by = by_form, design = subset(FYCdsgn, ACCELI42==1 & delay_ANY==1))
        
        res[["rsn_MD"]]  = 
          svyby(~afford_MD + insure_MD + other_MD,    
                FUN = func, by = by_form, design = subset(FYCdsgn, ACCELI42==1 & delay_MD==1))
        
        res[["rsn_DN"]]  = 
          svyby(~afford_DN + insure_DN + other_DN,    
                FUN = func, by = by_form, design = subset(FYCdsgn, ACCELI42==1 & delay_DN==1))
        
        res[["rsn_PM"]]  = 
          svyby(~afford_PM + insure_PM + other_PM,    
                FUN = func, by = by_form, design = subset(FYCdsgn, ACCELI42==1 & delay_PM==1))
      } 
      
      # For 2018 and beyond, CAPI changes only ask about affordability of treatment
      if(year >= 2018) {
        res[["no_afford"]] = 
          svyby(~afford_ANY + afford_MD + afford_DN + afford_PM, 
                FUN = func, by = by_form, design = subset(FYCdsgn, ACCELI42==1))
        
        res[["delayed"]] = 
          svyby(~delay_ANY + delay_MD + delay_DN + delay_PM, 
                FUN = func, by = by_form, design = subset(FYCdsgn, ACCELI42==1))
      }
      
      
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