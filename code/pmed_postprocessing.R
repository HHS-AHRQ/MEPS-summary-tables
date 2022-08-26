# PMED post-processing: 
#  - remove any suppressed rows
#  - remove RXDRGNAMs that were masked to therapeutic classes
#  - for 1996-2013, remove RXDRGNAM that do not show up in 2014-2019

for(year in yrs) {
  
  print(year)
  
  # Read in RX file to identify rows where all RXNDC are missing (-9 or -15).
  # This includes:
  #  - RXDRGNAM that have been masked to the therapeutic class level
  #  - some true RXDRGNAMs that are likely rare
  
  chkRX = read_MEPS(year = year, type = "RX")
  
  # For 1996-2013, merge with RX Multum Lexicon Addendum files
  if(year <= 2013) {
    Multum <- read_MEPS(year = year, type = "Multum") 
    chkRX <- chkRX %>%
      select(-starts_with("TC"), -one_of("PREGCAT", "RXDRGNAM")) %>%
      left_join(Multum, by = c("DUPERSID", "RXRECIDX"))
  }
  
  missingNDC = chkRX %>% 
    select(RXNDC, RXDRGNAM) %>% 
    mutate(hasNDC = (!RXNDC %in% c("-9", "-15"))) %>% 
    group_by(RXDRGNAM) %>% 
    summarize(hasNDC = sum(hasNDC)) %>% 
    filter(hasNDC == 0) %>% 
    pull(RXDRGNAM) %>% unique
  
  # Load in know Therapeutic class names
  knownTC = read.csv("dictionaries/pmed_classNames.csv") %>% pull(ClassName)
  
  
  # Load in formatted file ------------------------------------------------------
  
  csv_name = str_glue("formatted_tables/hc_pmed/DY{year}.csv")
  
  pmed_fmt = read.csv(csv_name)
  
  # Remove rows -----------------------------------------------------------------
  pmed_out = pmed_fmt %>%
    
    # Remove 'missing' values
    filter(!rowLevels %in% c("-9", "-15")) %>% 
    filter(!rowLevels %in% c("Not Ascertained")) %>% 
    
    # Remove suppressed rows   
    filter(value != "--") %>% 
    
    # Remove RXDRGNAMs that were masked to therapeutic classes
    filter(!(row_var == "RXDRGNAM" & rowLevels %in% knownTC)) 
  
  # Warn if any RXDRGNAMs have missing NDC but are NOT a known therapeutic class name
  chk_remaining = pmed_out %>% filter(rowLevels %in% missingNDC)
  
  if(nrow(chk_remaining) > 0) {
    print(" ----------   WARNING!! Some rows have all missing NDCs but are not listed in known TC file. Want to add these?")
    print(chk_remaining %>% select(Year, stat_group, stat_var, row_var, rowLevels, value, se, sample_size))
  }

  # Write out new file --------------------------------------------------------
  
  write_csv(pmed_out, file = csv_name)
  
}


# Create list of RXDRGNAMs that are in 2014-2019:
#  - running this AFTER excluding suppressed rows in these years
# 
# rxdrgnams = list()
# for(year in 2014:2019) {
# 
#   DY = str_glue("DY{year}")
#   csv_name = str_glue("formatted_tables/hc_pmed/{DY}.csv")
#   pmed_fmt = read.csv(csv_name)
# 
#   rxdrgnams[[DY]] = pmed_fmt %>%
#     filter(row_var == "RXDRGNAM") %>%
#     select(Year, rowLevels) %>%
#     distinct
# }
# 
# bind_rows(rxdrgnams) %>% write_csv(file = "dictionaries/pmed_RXDRGNAMs_2014-2019.csv")


# For 1996-2013, remove RXDRGNAM that do not show up in 2014-2019 -------------
#  - additional masking steps were implemented in 2013

edit_yrs = yrs[yrs < 2014]

keep_rxdrgnams = read.csv("dictionaries/pmed_RXDRGNAMs_2014-2019.csv") %>% 
  pull(rowLevels) %>% 
  unique

for(year in edit_yrs) {
  
  csv_name = str_glue("formatted_tables/hc_pmed/DY{year}.csv")
  pmed_fmt = read.csv(csv_name)
  
  pmed_out = pmed_fmt %>% 
    filter(row_var == "TC1name" | rowLevels %in% keep_rxdrgnams)
  
  # Write out new file 
  write_csv(pmed_out, file = csv_name)
}





