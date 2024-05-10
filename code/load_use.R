
# Load event files
  RX  <- read_MEPS(year = year, type = "RX") %>% rename(EVNTIDX = LINKIDX) %>% zap_labels()
  DVT <- read_MEPS(year = year, type = "DV") %>% zap_labels()
  IPT <- read_MEPS(year = year, type = "IP") %>% zap_labels()
  ERT <- read_MEPS(year = year, type = "ER") %>% zap_labels()
  OPT <- read_MEPS(year = year, type = "OP") %>% zap_labels()
  OBV <- read_MEPS(year = year, type = "OB") %>% zap_labels()
  HHT <- read_MEPS(year = year, type = "HH") %>% zap_labels()

# Define sub-levels for office-based and outpatient
  
  if(year < 2018) {
    OBV$seedoc_var = OBV$SEEDOC    
    OPT$seedoc_var = OPT$SEEDOC
  } else{
    OBV$seedoc_var = OBV$SEEDOC_M18    
    OPT$seedoc_var = OPT$SEEDOC_M18
  }
  
  OBV <- OBV %>%
    mutate(event_v2X = recode_factor(
      seedoc_var, 
      .default = 'Missing', .missing = "Missing", 
      '1' = 'OBD', '2' = 'OBO'))
  
  OPT <- OPT %>%
    mutate(event_v2X = recode_factor(
      seedoc_var, 
      .default = 'Missing', .missing = "Missing", 
      '1' = 'OPY', '2' = 'OPZ'))

# Stack events ----------------------------------------------------------------
 
  sop_vars <- c("SF", "MR", "MD", "PV", "VA", "TR", "OF", "SL", "WC", "OT", "XP")
  
  # Add 'total' (dr + facility) payments for hosp events.
  ERT <- ERT %>% add_tot_sops("ER", yr, sop_vars)
  OPT <- OPT %>% add_tot_sops("OP", yr, sop_vars)
  IPT <- IPT %>% add_tot_sops("IP", yr, sop_vars)
  
  # Remove event prefix from SOP colnames
  RX  <- RX  %>% rm_evt_key("RX", yr, sop_vars) 
  DVT <- DVT %>% rm_evt_key("DV", yr, sop_vars) 
  IPT <- IPT %>% rm_evt_key("IP", yr, sop_vars) 
  ERT <- ERT %>% rm_evt_key("ER", yr, sop_vars) 
  OPT <- OPT %>% rm_evt_key("OP", yr, sop_vars) 
  OBV <- OBV %>% rm_evt_key("OB", yr, sop_vars) 
  HHT <- HHT %>% rm_evt_key("HH", yr, sop_vars) 
  
  # Stack events
  keep_vars <- 
    c("event", "EVNTIDX", "DUID", "PID", "DUPERSID", "PANEL", "IMPFLAG",
      'seedoc_var','event_v2X',
      str_glue("{sop_vars}{yr}X"),
      str_glue("PERWT{yr}F"), "VARSTR", "VARPSU") 
  
  stacked_events <- bind_rows(
    RX %>% mutate(event = "RX"), 
    DVT %>% mutate(event = "DVT"), 
    IPT %>% mutate(event = "IPT"), 
    ERT %>% mutate(event = "ERT"), 
    OPT %>% mutate(event = "OPT"), 
    OBV %>% mutate(event = "OBV"), 
    HHT %>% mutate(event = "HHT")
  ) %>% 
    select(all_of(keep_vars))
  
  
# Remove 'yr' from colnames
  colnames(stacked_events) <- colnames(stacked_events) %>% gsub(yr,"",.)
  
  stacked_events <- stacked_events %>%
    mutate(PRX = PVX + TRX,
           # OZX = OFX + SLX + OTX + ORX + OUX + WCX + VAX, -- oth public/private dont exist anymore
           OZX = OFX + SLX + OTX + WCX + VAX) %>% 
    select(DUPERSID, event, event_v2X, seedoc_var,
           XPX, SFX, MRX, MDX, PRX, OZX)
  
# Add demographic and survey vars from FYC file
  EVENTS <- stacked_events %>% full_join(FYCsub, by = 'DUPERSID')
