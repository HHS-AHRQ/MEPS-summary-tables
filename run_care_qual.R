# Create directory ------------------------------------------------------------

tbl_dir <- "data_tables/hc_care_qual"
dir.create(tbl_dir)

# Define groups ---------------------------------------------------------------
row_grps <- demo_grps %>% add_v2X

# Run for specified year(s) ---------------------------------------------------

qual_years <- year_list[year_list >= 2002]
qual_years <- qual_years[qual_years <= 2017 | is.odd(qual_years)]

for(year in qual_years) {
  
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
      
      # Adult quality of care variables

      res[["adult_routine"]] = 
        svyby(~adult_routine, FUN = func, by = by_form, 
              design = subset(SAQdsgn, ADRTCR42==1 & AGELAST >= 18))
  
      res[["adult_illness"]] = 
        svyby(~adult_illness, FUN = func, by = by_form, 
              design = subset(SAQdsgn, ADILCR42==1 & AGELAST >= 18))
      
      adult_dsgn <- subset(SAQdsgn, ADAPPT42 >= 1 & AGELAST >= 18)
      res[["adult_time"]]    = svyby(~adult_time,    FUN = func, by = by_form, design = adult_dsgn)
      res[["adult_listen"]]  = svyby(~adult_listen,  FUN = func, by = by_form, design = adult_dsgn)
      res[["adult_rating"]]  = svyby(~adult_rating,  FUN = func, by = by_form, design = adult_dsgn)
      res[["adult_respect"]] = svyby(~adult_respect, FUN = func, by = by_form, design = adult_dsgn)
      res[["adult_explain"]] = svyby(~adult_explain, FUN = func, by = by_form, design = adult_dsgn)

      
      # Child quality of care variables (only odd years starting 2017, except child_dental)
    
      res[["child_routine"]] = 
        svyby(~child_routine, FUN = func, by = by_form, 
              design = subset(FYCdsgn, CHRTCR42==1 & AGELAST < 18))
      
      res[["child_illness"]] = 
        svyby(~child_illness, FUN = func, by = by_form, 
              design = subset(FYCdsgn, CHILCR42==1 & AGELAST < 18))
      
      child_dsgn <- subset(FYCdsgn, CHAPPT42 >= 1 & AGELAST < 18)
      res[["child_time"]]    = svyby(~child_time,    FUN = func, by = by_form, design = child_dsgn)
      res[["child_listen"]]  = svyby(~child_listen,  FUN = func, by = by_form, design = child_dsgn)
      res[["child_rating"]]  = svyby(~child_rating,  FUN = func, by = by_form, design = child_dsgn)
      res[["child_respect"]] = svyby(~child_respect, FUN = func, by = by_form, design = child_dsgn)
      res[["child_explain"]] = svyby(~child_explain, FUN = func, by = by_form, design = child_dsgn)
   
      
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