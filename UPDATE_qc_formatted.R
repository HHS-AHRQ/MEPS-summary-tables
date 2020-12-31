

# THIS CODE CAN BE DELETED ON 7/1/2021 -- USED TO CHECK RE-FACTOR FOR TRANSITION TO TABLEAU



# QC tables ------------------------------------------------------------------

# No messages = No issues!

rm(list = ls())

QC <- function(name, qc_df) {
  if(nrow(qc_df) > 0) {
    cat("CHECK ", name)
    print(qc_df)
  }
}

# Define appKey and years ----------------------------------------------------
#appKey = "hc_ins";  years = 1996:2018;
#appKey = "hc_pmed"; years = 1996:2018;
#appKey = "hc_use";  years = c(1996, 2000, 2002, 2005, 2015:2018);
#appKey = "hc_cond_icd9";  years = 1996:2015;
#appKey = "hc_cond_icd10";  years = 2016:2018;


#appKey = "hc_care_access";  years = 2002:2018; ## !! 2018 is tricky...
#appKey = "hc_care_diab";  years = 2002:2018; 
appKey = "hc_care_qual";  years = 2002:2017; 

# year = 2018



# Loop through years ---------------------------------------------------------

for(year in years) { print(year)

  options(dplyr.width = Inf)

  new  <- read.csv(str_glue("formatted_tables/{appKey}/DY{year}.csv"))

  # hc_care -- load subset
  if(appKey %>% startsWith("hc_care")) {
    orig_care <- read.csv(str_glue("formatted_tables - orig/hc_care/DY{year}.csv"))

    if(appKey == "hc_care_access") {
      orig <- orig_care %>% filter(grepl("Access", col_group))
      new <- new %>%
        mutate(col_label = gsub( " (2002-2017)","", col_label, fixed = T))
    }

    if(appKey == "hc_care_diab") orig <- orig_care %>% filter(grepl("Diabetes", col_group))
    if(appKey == "hc_care_qual") orig <- orig_care %>% filter(grepl("Quality", col_group))

  } else {
    orig <- read.csv(str_glue("formatted_tables - orig/{appKey}/DY{year}.csv"))
  }

  # Edits
  orig <- orig %>% rename(value = coef)

  new_tbl  <- new  %>% as_tibble
  orig_tbl <- orig %>% as_tibble

  if("row_group" %in% colnames(orig_tbl)) {
    orig_tbl <- orig_tbl %>%
      mutate(row_group = replace(row_group, row_group == "", "(none)"))
  }
  if("col_group" %in% colnames(orig_tbl)) {
    orig_tbl <- orig_tbl %>%
      mutate(col_group = replace(col_group, col_group == "", "(none)"))
  }

  if(appKey == "hc_use") {
    new_tbl <- new_tbl %>% filter(!(col_var == row_var & row_var != "ind"))
  }
  
  
  if(appKey == "hc_care_qual") {
    orig_tbl <- orig_tbl %>%
      separate(col_var, into = c("adult_child", "col_var")) %>%
      separate(col_group, into = c("col_group", "drop"), sep = ":") %>%
      select(-drop)
  }
  
  if(appKey %>% startsWith("hc_care")) {
    orig_tbl <- orig_tbl %>%
      mutate(caption = gsub(" by", ", by", caption))
  }
  
  
  # Compare caption only
  cap_vars <- c("adult_child", "stat_var", "row_var", "col_var", "caption")
  captions <- suppressMessages(full_join(
    orig_tbl %>% select(one_of(cap_vars)) %>% rename(orig_caption = caption) %>% distinct,
    new_tbl %>% select(one_of(cap_vars)) %>% rename(new_caption = caption) %>% distinct))

  QC("caption_diff",captions %>%
    filter(orig_caption != new_caption))

  QC("caption_missing", captions %>%
    filter(is.na(orig_caption) | is.na(new_caption)))

  # Edits based on apps
  if(appKey == "hc_pmed") {
    orig_tbl <- orig_tbl %>%
      mutate(col_label = "(none)", rowLevels = ifelse(row_var == "RXDRGNAM", toupper(rowLevels), rowLevels))
    #new_tbl <- new_tbl %>% filter(value != "--")
  }


  # Combine and compare
  combined <- suppressMessages(full_join(
    orig_tbl %>% rename(value_orig = value, se_orig = se) %>% mutate(in_orig = TRUE),
    new_tbl  %>% rename(value_new = value,  se_new = se)  %>% mutate(in_new = TRUE)))

  # Check number of rows
  nc <- nrow(combined)
  norig <- nrow(orig_tbl)
  nnew  <- nrow(new_tbl)

  if(nc != nnew) {
    print("WARNING: DIFFERENT NUMBER OF ROWS")
  }

  if(FALSE) {
    # should have 0 rows
    QC("QC1", combined %>%
         filter(is.na(in_orig) | is.na(in_new)) %>%
         count(col_var, row_var, in_orig, in_new))

    QC("QC2", combined %>%
         filter(is.na(in_orig) | is.na(in_new)) %>%
         count(stat_var, stat_label,  in_orig, in_new))


  }


  QC("QC1.5", combined %>%
    filter(is.na(in_orig) | is.na(in_new), value_new != "--")
  )


  combined2 <- combined %>%
    mutate(
      value_new = suppressWarnings(as.numeric(value_new)),
      se_new    = suppressWarnings(as.numeric(se_new)),

      value_new = ifelse(Percent, value_new*100, value_new),
      se_new    = ifelse(Percent, se_new*100, se_new),

      asterisk_orig = grepl("*", value_orig, fixed = T),

      orig_num = value_orig %>%
        gsub(",", "", .) %>%
        gsub("*","", ., fixed = T) %>%
        as.numeric,

      se_num = se_orig %>%
        gsub(",", "", .) %>%
        gsub("*", "", ., fixed = T) %>%
        as.numeric(),

      n_digits  = nchar(word(orig_num, 2, sep = "\\.")),
      se_digits = nchar(word(se_num,   2, sep = "\\."))) %>%

    replace_na(list(n_digits = 0, se_digits = 0))

  combined3 <- combined2 %>%
    select(-ends_with("label"), -ends_with("group"), -caption) %>%
    mutate(
      value_rnd = round(value_new, n_digits),
      se_rnd    = round(se_new,   se_digits)
    )


  # These are bad:
  QC("QC3", combined3 %>% filter(value_rnd != orig_num))
  QC("QC4", combined3 %>% filter(se_rnd != se_num))

  QC("QC5", combined3 %>% filter(is.na(in_new)))
  QC("QC6", combined3 %>% filter(is.na(in_orig) & !is.na(value_new)))


  # These are also bad:
  QC("QC7", combined3 %>% filter(is.na(value_rnd) & !is.na(orig_num), sample_size >= 60))
  QC("QC8", combined3 %>% filter(!is.na(value_rnd) & is.na(orig_num), sample_size >= 60))

  QC("QC9", combined3 %>% filter(is.na(se_rnd)  & !is.na(se_num), sample_size >= 60))
  QC("QC10", combined3 %>% filter(!is.na(se_rnd) & is.na(se_num), sample_size >= 60))

  QC("QC11", combined3 %>% filter(asterisk == "*" & !asterisk_orig, sample_size >= 60))
  QC("QC12", combined3 %>% filter(asterisk == ""  & asterisk_orig, sample_size >= 60))

  QC("QC13", combined3 %>% filter(is.na(value_rnd) & !is.na(se_rnd), sample_size >= 60))
  QC("QC14", combined3 %>% filter(!is.na(value_rnd) & is.na(se_rnd), sample_size >= 60))

} # end for year in years


# # These are OK:
# combined3 %>% filter(is.na(value_rnd) & is.na(orig_num))
# combined3 %>% filter(asterisk == "*")

# View(combined3)
