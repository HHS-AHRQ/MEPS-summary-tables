# Install and load packages
  package_names <- c("survey","dplyr","foreign","devtools")
  lapply(package_names, function(x) if(!x %in% installed.packages()) install.packages(x))
  lapply(package_names, require, character.only=T)

  install_github("e-mitchell/meps_r_pkg/MEPS")
  library(MEPS)

  options(survey.lonely.psu="adjust")

# Load FYC file
  FYC <- read_sas('C:/MEPS/.FYC..sas7bdat');
  year <- .year.

  if(year <= 2001) FYC <- FYC %>% mutate(VARPSU = VARPSU.yy., VARSTR=VARSTR.yy.)
  if(year <= 1998) FYC <- FYC %>% rename(PERWT.yy.F = WTDPER.yy.)
  if(year == 1996) FYC <- FYC %>% mutate(AGE42X = AGE2X, AGE31X = AGE1X)

  FYC <- FYC %>%
    mutate_at(vars(starts_with("AGE")),funs(replace(., .< 0, NA))) %>%
    mutate(AGELAST = coalesce(AGE.yy.X, AGE42X, AGE31X))

  FYC$ind = 1  

# Perceived health status
  if(year == 1996)
    FYC <- FYC %>% mutate(RTHLTH53 = RTEHLTH2, RTHLTH42 = RTEHLTH2, RTHLTH31 = RTEHLTH1)

  FYC <- FYC %>%
    mutate_at(vars(starts_with("RTHLTH")), funs(replace(., .< 0, NA))) %>%
    mutate(
      health = coalesce(RTHLTH53, RTHLTH42, RTHLTH31),
      health = recode_factor(health, .default = "Missing", .missing = "Missing", 
        "1" = "Excellent",
        "2" = "Very good",
        "3" = "Good",
        "4" = "Fair",
        "5" = "Poor"))

# Keep only needed variables from FYC
  FYCsub <- FYC %>% select(health,ind, DUPERSID, PERWT.yy.F, VARSTR, VARPSU)

# Load event files
  RX <- read_sas('C:/MEPS/.RX..sas7bdat')
  DVT <- read_sas('C:/MEPS/.DV..sas7bdat')
  IPT <- read_sas('C:/MEPS/.IP..sas7bdat')
  ERT <- read_sas('C:/MEPS/.ER..sas7bdat')
  OPT <- read_sas('C:/MEPS/.OP..sas7bdat')
  OBV <- read_sas('C:/MEPS/.OB..sas7bdat')
  HHT <- read_sas('C:/MEPS/.HH..sas7bdat')

# Define sub-levels for office-based and outpatient
#  To compute estimates for these sub-events, replace 'event' with 'event_v2X'
#  in the 'svyby' statement below, when applicable
  OBV <- OBV %>%
    mutate(event_v2X = recode_factor(
      SEEDOC, .default = 'Missing', .missing = "Missing", '1' = 'OBD', '2' = 'OBO'))

  OPT <- OPT %>%
    mutate(event_v2X = recode_factor(
      SEEDOC, .default = 'Missing', .missing = "Missing", '1' = 'OPY', '2' = 'OPZ'))

# Stack events
  stacked_events <- stack_events(RX, DVT, IPT, ERT, OPT, OBV, HHT,
    keep.vars = c('SEEDOC','event_v2X'))

  stacked_events <- stacked_events %>%
    mutate(event = data,
           PR.yy.X = PV.yy.X + TR.yy.X,
           OZ.yy.X = OF.yy.X + SL.yy.X + OT.yy.X + OR.yy.X + OU.yy.X + WC.yy.X + VA.yy.X) %>%
    select(DUPERSID, event, event_v2X, SEEDOC,
      XP.yy.X, SF.yy.X, MR.yy.X, MD.yy.X, PR.yy.X, OZ.yy.X)

# Create datasets for physician office-based / outpatient events
  OBD = OBV %>% filter(event_v2X == "OBD")
  OPY = OPT %>% filter(event_v2X == "OPY")

  events <- c("DVT", "RX",  "OBV", "OBD", "OPT",
              "OPY", "ERT", "IPT", "HHT")

# Run for each event dataset
  results <- list()
  for(ev in events) {
    key <- ev
    df <- get(key) %>%
      rm_evnt_key() %>%
      add_total_sops() %>%
      mutate(PR.yy.X = PV.yy.X + TR.yy.X,
             OZ.yy.X = OF.yy.X + SL.yy.X + OT.yy.X + OR.yy.X + OU.yy.X + WC.yy.X + VA.yy.X)

    pers_events <- df %>%
      group_by(DUPERSID) %>%
      summarise(ANY = sum(XP.yy.X >= 0),
                EXP = sum(XP.yy.X > 0),
                SLF = sum(SF.yy.X > 0),
                MCR = sum(MR.yy.X > 0),
                MCD = sum(MD.yy.X > 0),
                PTR = sum(PR.yy.X > 0),
                OTZ = sum(OZ.yy.X > 0))

    n_events <- full_join(pers_events,FYCsub,by="DUPERSID") %>%
      mutate_at(vars(ANY, EXP, SLF, MCR, MCD, PTR, OTZ),
                function(x) ifelse(is.na(x),0,x))

    EVdsgn <- svydesign(
      id = ~VARPSU,
      strata = ~VARSTR,
      weights = ~PERWT.yy.F,
      data = n_events,
      nest = TRUE)

    results[[key]] <- svyby(~ANY, by = ~health, FUN = svymean, design = EVdsgn)
  }

print(results)