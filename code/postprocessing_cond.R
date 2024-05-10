# COND post-processing:
# - Add CCSR BODY system to tables
# - Add '**' to Coronavirus for footnote
# - Only for data years 2016 and later



# Read in BODY system from CCSR crosswalk

condition_codes <- read_csv(ccsr_url) %>%
  setNames(c("CCSR", "CCSR_desc", "Condition", "Body_System"))

body_labels = condition_codes %>% 
  separate(Body_System, into = c("BODY_SYSTEM", "BODY_LABEL"), sep = ": ") %>% 
  distinct(Condition, BODY_SYSTEM, BODY_LABEL)


# OLD -------------
# cdir <- "C:/Users/emily.mitchell/OneDrive - HHS Office of the Secretary/Desktop/AHRQ/MEPS/GROUPS/COND/DXCCSR_collapsed"
# 
# body_system <- read_excel(
#   str_glue("{cdir}/CCSR_CCSRMATCH_xwalk_2023-05-26.xlsx"), sheet = 2) %>% 
#   filter(!is.na(BODY_SYSTEM)) %>%
#   distinct(BODY_SYSTEM, CCSRMATCH_DESCRIPTION)
# 

# Read in BODY labels (i.e. ICD10 chapters) from dictionaries folder
# body_labels = read_excel("dictionaries/CCSR_ICD10_chapters.xlsx") %>% 
#   setNames(c("BODY_LABEL", "BODY_SYSTEM", "ICD10_chapter", "ICD10_codes")) %>% 
#   distinct(BODY_SYSTEM, BODY_LABEL)
# # combining PNL and PRG since the two are often conflated in MEPS reporting
# body_labels = body_labels %>% 
#   add_row(
#     BODY_SYSTEM = "PNL/PRG", 
#     BODY_LABEL = "Pregnancy, childbirth, puerperium and perinatal conditions"
#   )
#  --------------




for(year in yrs) {
  print(year)
  
  # Load in formatted file -----------------------------------------------------
  csv_name = str_glue("formatted_tables/hc_cond_icd10/DY{year}.csv")
  
  cond_fmt = read.csv(csv_name) %>%
    
    # !! fix typo in crosswalk -- add 'the'
    mutate(
      rowLevels = replace(
        rowLevels, 
        rowLevels == "Other conditions of circulatory system",
        "Other conditions of the circulatory system")
    )
  
  # Merge BODY system onto file
  cond_fmt2 = cond_fmt %>% 
   # left_join(body_system, by = c("rowLevels" = "CCSRMATCH_DESCRIPTION")) %>% 
    left_join(body_labels, by = c("rowLevels" = "Condition"))  %>% 
    
    # Add '**' to COVID for extra footnote
    mutate(
      rowLevels = ifelse(
        rowLevels == "Coronavirus disease (COVID-19)", 
        "Coronavirus disease (COVID-19)**", rowLevels)
    )
  
  
  # QC
  print("This should be 0 rows...")
  cond_fmt2 %>% 
    filter(is.na(BODY_SYSTEM)) %>% 
    count(rowLevels) %>% 
    print(n = 10)
  
  
  # Write out new file --------------------------------------------------------
  
  write_csv(cond_fmt2, file = csv_name, na = "")
  
}
