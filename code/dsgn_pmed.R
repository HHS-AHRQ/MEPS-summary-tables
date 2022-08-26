# Merge onto FYC file to capture all VARSTR, VARPSU values before defining svydesign

RX_fyc = full_join(
  RX  %>% select(-VARSTR, -VARPSU, -PERWTF) %>% mutate(inRX = TRUE), 
  FYC %>% select( VARSTR,  VARPSU,  PERWTF, DUPERSID), by = "DUPERSID")


# Summarize to person-drug-level
DRGpers <- RX_fyc %>%
  group_by(DUPERSID, VARSTR, VARPSU, PERWTF, RXDRGNAM, inRX) %>%
  summarise(n_RX = sum(count), RXXPX = sum(RXXPX)) %>%
  mutate(count = 1) %>%
  ungroup

DRGdsgn <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWTF,
  data = DRGpers,
  nest = TRUE
) %>% subset(inRX)


# Summarize to person-TC1-level
TC1pers <- RX_fyc %>%
  group_by(DUPERSID, VARSTR, VARPSU, PERWTF, TC1name, inRX) %>%
  summarise(n_RX = sum(count), RXXPX = sum(RXXPX)) %>%
  mutate(count = 1) %>%
  ungroup

TC1dsgn <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWTF,
  data = TC1pers,
  nest = TRUE
) %>% subset(inRX)
