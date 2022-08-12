
# Load conditions PUF file
  cond_puf <- read_MEPS(year = year, type = "COND") %>%
    select(DUPERSID, CONDIDX, starts_with("CC")) %>%
    zap_labels()


# Merge condition codes

  if(year < 2016) {
    condition_codes <- read_csv(ccs_url) %>%
      setNames(c("CCS", "CCS_desc", "Condition")) %>%
      mutate(CCS = as.numeric(CCS))
    
    cond <- cond_puf %>% 
      mutate(CCS = as.numeric(as.character(CCCODEX))) %>%
      left_join(condition_codes, by = "CCS")
    
  } else {
    
    ## TEMPORARY!! UN-COMMENT THIS BEFORE RUNNING NEXT
    condition_codes <- read_csv(ccsr_url) %>%
      setNames(c("CCSR", "CCSR_desc", "Condition"))

    
    ## TEMPORARY!! TESTING OUT NEW CCSRM CODES --
    # cdir <- "C:/Users/emily.mitchell/OneDrive - HHS Office of the Secretary/Desktop/AHRQ/MEPS/GROUPS/COND/DXCCSR_collapsed"
    # condition_codes <- read_csv(
    #   str_glue("{cdir}/CCSR_CCSRMATCH_xwalk_2022-07-22.csv")) %>%
    #   setNames(c("CCSR", "CCSR_desc", "Condition")) # %>%
    #  # select(-Notes)
    
    # Convert multiple CCSRs to separate lines 
    cond <- cond_puf %>% 
      gather(CCSRnum, CCSR, CCSR1X:CCSR3X) %>% 
      filter(CCSR != "") %>%
      left_join(condition_codes)
  }
  

# Load event files
  RX  <- read_MEPS(year = year, type = "RX") %>% rename(EVNTIDX = LINKIDX) %>% zap_labels()
  IPT <- read_MEPS(year = year, type = "IP") %>% zap_labels()
  ERT <- read_MEPS(year = year, type = "ER") %>% zap_labels()
  OPT <- read_MEPS(year = year, type = "OP") %>% zap_labels()
  OBV <- read_MEPS(year = year, type = "OB") %>% zap_labels()
  HHT <- read_MEPS(year = year, type = "HH") %>% zap_labels()
  
# Load event-condition linking file
  clink1 <- read_MEPS(year = year, type = "CLNK") %>%
    select(DUPERSID, CONDIDX, EVNTIDX) %>%
    zap_labels()

# Stack events (condition data not collected for DN and OM events)
  
  sop_vars <- c("SF", "MR", "MD", "PV", "VA", "TR", "OF", "SL", "WC", "OT", "XP")
  
  # Add 'total' (dr + facility) payments for hosp events.
  ERT <- ERT %>% add_tot_sops("ER", yr, sop_vars)
  OPT <- OPT %>% add_tot_sops("OP", yr, sop_vars)
  IPT <- IPT %>% add_tot_sops("IP", yr, sop_vars)
   
  # Remove event prefix from SOP colnames
  RX  <- RX  %>% rm_evt_key("RX", yr, sop_vars) 
  IPT <- IPT %>% rm_evt_key("IP", yr, sop_vars) 
  ERT <- ERT %>% rm_evt_key("ER", yr, sop_vars) 
  OPT <- OPT %>% rm_evt_key("OP", yr, sop_vars) 
  OBV <- OBV %>% rm_evt_key("OB", yr, sop_vars) 
  HHT <- HHT %>% rm_evt_key("HH", yr, sop_vars) 
  
  # Stack events
  keep_vars <- 
    c("event", "EVNTIDX", "DUID", "PID", "DUPERSID", "PANEL", "IMPFLAG",
      str_glue("{sop_vars}{yr}X"),
      str_glue("PERWT{yr}F"), "VARSTR", "VARPSU") 
  
  stacked_events <- bind_rows(
    RX %>% mutate(event = "RX"), 
    IPT %>% mutate(event = "IPT"), 
    ERT %>% mutate(event = "ERT"), 
    OPT %>% mutate(event = "OPT"), 
    OBV %>% mutate(event = "OBV"), 
    HHT %>% mutate(event = "HHT")
  ) %>% select(all_of(keep_vars))

  
# Remove 'yr' from colnames
  colnames(stacked_events) <- colnames(stacked_events) %>% gsub(yr,"",.)
  
  stacked_events <- stacked_events %>%
    mutate(PRX = PVX + TRX,
          # OZX = OFX + SLX + OTX + ORX + OUX + WCX + VAX, -- oth public/private dont exist anymore
           OZX = OFX + SLX + OTX + WCX + VAX
           ) %>%
    select(DUPERSID, event, EVNTIDX,
           XPX, SFX, MRX, MDX, PRX, OZX)

  
# Merge link file
  cond <- cond %>%
    full_join(clink1, by = c("DUPERSID", "CONDIDX")) %>%
    distinct(DUPERSID, EVNTIDX, Condition, .keep_all=T)
  
# Merge events with conditions-link file and FYCsub
  all_events <- full_join(stacked_events, cond, by = c("DUPERSID", "EVNTIDX")) %>%
    filter(!is.na(Condition), XPX >= 0) %>%
    mutate(count = 1) %>%
    full_join(FYCsub, by = "DUPERSID")
