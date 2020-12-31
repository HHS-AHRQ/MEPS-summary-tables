# -----------------------------------------------------------------------------
# Emily Mitchell
# Updated: 12/28/2020
# 
# MEPS tables creation - format tables
#  - Function to format MEPS tables for new data years
# -----------------------------------------------------------------------------

# Changes from previous version:
#  - changed 'coef' to 'value'
#  - removed comma and '*' from coef/value column and added 'asterisk' column
#  - convert Percents back to decimals (between 0 and 1) and added 'Percent' column
#  - kept adjusted numbers and included in caption (e.g. 'in thousands')
#  - adding 'Year', 'stat_group', 'col_group', 'row_group', 'asterisk', 'Percent', 'sample_size' to all tables -- although some will not be used
#  - leaving suppressed values in the tables (most relevant for hc_pmed and hc_cond tables)
#  - leaving redundant categories in 'hc_use' (e.g. agegrps x agegrps)
#  - split hc_care into hc_care_access, hc_care_diab, and hc_care_qual -- these should be displayed as 3 separate dashboards
#  - for hc_care_qual, added a column to indicate adult vs child ('adult_child'). This should be included as a dropdown select in Tableau


format_tables = function(appKey, years) {

  # Define and create directories ---------------------------------------------
  dir <- str_glue("data_tables/{appKey}")
  
  fmt_dir <- str_glue("formatted_tables/{appKey}")
  dir.create(fmt_dir, showWarnings = F)
  
  has_nexp <- any(grepl("n_exp.csv", list.files(dir, recursive = T)))
  
  
  # Load labels and dictionaries ----------------------------------------------
  stat_labels  <- read_excel("dictionaries/stat_labels.xlsx")
  group_labels <- read_excel("dictionaries/group_labels.xlsx")
  level_labels <- read_excel("dictionaries/level_labels.xlsx")
  
  # Get order for factors later
  stat_order <- stat_labels  %>% pull(stat_var) %>% unique
  grp_order  <- group_labels %>% pull(var) %>% unique
  
  # Keep only app-specific versions (if applicable) 
  stat_labels <- stat_labels %>% 
    arrange(desc(table_series)) %>%
    filter(table_series %in% c(appKey, "(all)")) %>%
    distinct(stat_var, .keep_all = T) %>%
    select(-table_series)
  
  group_labels <- group_labels %>% 
    arrange(desc(table_series)) %>%
    filter(table_series %in% c(appKey, "(all)")) %>%
    distinct(var, .keep_all = T) %>%
    select(-table_series)
  
  
  # Loop through years --------------------------------------------------------

  for(year in years) { cat(year,"..")

    yr_dir  <- str_glue("{dir}/{year}")
    
    out_dir <- str_glue("{fmt_dir}/DY{year}.csv")
    
    stats <- list.files(yr_dir) %>% 
      gsub(".csv", "", . , fixed = T) %>%
      pop(c("n", "n_exp")) 
    

    # Loop through stats and stack --------------------------------------------
    tbs <- list()
    for(stat in stats) {
      print(stat)
      tbs[[stat]]  <-
        read.csv(str_glue("{dir}/{year}/{stat}.csv")) %>%
        mutate(stat_var = stat) %>%
        rename(value = stat, se = str_glue("{stat}_se"))
    }
    
    # Stack, remove '_v2' suffix, and add level labels
    all_tbls <- bind_rows(tbs) %>% rm_v2 %>% add_keys(level_labels)
    
    
    # Merge sample sizes for suppression rules --------------------------------
    n_df <- read.csv(str_glue("{dir}/{year}/n.csv")) %>% 
      rm_v2 %>% add_keys(level_labels)
    
    if(has_nexp) {
      n_exp <- read.csv(sprintf("%s/%s/n_exp.csv", dir, year)) %>% 
        rm_v2 %>% add_keys(level_labels) 
    }
    
    # For hc_use, need all row x col and col x row 
    if(appKey == "hc_use"){
      all_tbls <- bind_rows(all_tbls, switch_labels(all_tbls)) %>% distinct
      n_df     <- bind_rows(n_df,     switch_labels(n_df))     %>% distinct
      n_exp    <- bind_rows(n_exp,    switch_labels(n_exp))    %>% distinct
    }
    
    all_tbls <- all_tbls %>% left_join(n_df)
    
    if(has_nexp) 
      all_tbls <- all_tbls %>% left_join(n_exp)
    
    
    # Format tables -----------------------------------------------------------
    
    fmt_tbls <- all_tbls %>% as_tibble %>% 
      
      mutate(Year = year) %>%
      
      # Remove 'OBO' and 'OPZ' -- no longer including non-physician events
      filter(
        !colLevels %in% c('OBO', 'OPZ'),
        !rowLevels %in% c('OBO', 'OPZ')) %>%
      
      # Remove 'Missing' and 'Inapplicable' groups
      filter(
        !rowLevels %in% c("Missing", "Inapplicable"),
        !colLevels %in% c("Missing", "Inapplicable")) %>%
      
      # Merge adjustment denominators and suppress where needed
      left_join(stat_labels) %>%
      
      mutate(sample_size = ifelse(value %in% c("meanEXP","medEXP"), n_exp, n)) %>%
      
      mutate(RSE = se/value,
             Percent = (stat_var %>% startsWith("pct")),
             special_pct = (Percent & (value < 0.1) & (RSE < (0.1/value-1)/1.96)),
             suppress = (RSE > 0.5),
             suppress = ifelse(special_pct, FALSE, suppress),
             suppress = ifelse(sample_size < 60 | se == 0, TRUE, suppress),
             # suppress = (sample_size < 60 | RSE > 0.5) | (se == 0),
             # suppress = replace(suppress, special_pct, FALSE),
             asterisk = ifelse(RSE > 0.3 & !suppress, "*", "")) %>%
      
      mutate(value = ifelse(suppress, NA, value/denom),
             se    = ifelse(suppress, NA, se/denom))  %>%
      
      select(Year, stat_group, stat_var, stat_label,
             rowGrp, rowLevels, colGrp, colLevels, value, se,
             asterisk, Percent, sample_size, adjText)

    
    # Add total categories for 'All people' and 'All events' ------------------
    fmt_totals <- fmt_tbls %>%
      add_totals('row') %>%
      add_totals('col') 
    
    
    # For hc_care_qual, split 'adult' and 'child' into separate column --------
    if(appKey == "hc_care_qual") {
      fmt_totals <- fmt_totals %>%
        separate(colGrp, into = c("adult_child", "colGrp"), sep = "_")
    }
    
    # Add row/col labels and groups -------------------------------------------
    row_labels <- group_labels %>% 
      rename(row_group = group, rowGrp = var, 
             row_label = label, row_caption = var_caption)
    
    col_labels <- group_labels %>% 
      rename(col_group = group, colGrp = var, 
             col_label = label, col_caption = var_caption)
    
    fmt_labelled <- fmt_totals %>%
      left_join(row_labels) %>%
      left_join(col_labels)
 
    
    
    # Add caption -------------------------------------------------------------
    
    # Initialize statText
    fmt_captioned <- fmt_labelled %>% mutate(statText = "")
    
    # For 'hc_use', add qualifier (statText) for Number of people with event/sop
    if(appKey == "hc_use") {
      fmt_captioned <- fmt_captioned %>%
        mutate(
          by_evt = (rowGrp == "event" | colGrp == "event"),
          by_sop = (rowGrp == "sop"   | colGrp == "sop"),
          
          statText = ifelse(stat_var == "totPOP" & by_evt, "with an event",   statText),
          statText = ifelse(stat_var == "totPOP" & by_sop, "with an expense", statText))
    }
    
    # For 'hc_care' tables, use slightly different captioning (from group_labels.xlsx)
    if(appKey %>% startsWith("hc_care")) {
      
      fmt_captioned <- fmt_captioned %>%
        mutate(
          
          byGrps = str_glue("by {row_label}") %>% tolower %>% 
            gsub("by (none)" , "", ., fixed = T),
          
          stat_caption = str_glue("{stat_label} {adjText} (standard errors)", .na = ""),
          
          caption = str_glue("{col_caption}, {byGrps}", .na = "") %>%
            str_replace_all(fixed("[number/percent]"), stat_caption)
        )
      
      # For 'hc_care_qual', replace '[adults/children]' placeholder with appropriate text
      if(appKey == "hc_care_qual") {
        fmt_captioned <- fmt_captioned %>%
          mutate(
            adult_child_text = ifelse(adult_child == "adult", "adults", "children"),
            caption = caption %>% str_replace_all(fixed("[adults/children]"), adult_child_text)
          ) 
      }
          

    } else {
      
      fmt_captioned <- fmt_captioned %>%
        mutate(
          byGrps = str_glue("by {row_label} and {col_label}") %>% tolower %>% 
            gsub("by (none) and", "by", ., fixed = T) %>% 
            gsub("and (none)", "", ., fixed = T) %>% 
            gsub("by (none)" , "", ., fixed = T),
      
          caption = str_glue(
            "{stat_label} {adjText} {statText} (standard errors) {byGrps}", .na = "")
        ) 
    }
    
    # Clean up
    fmt_captioned <- fmt_captioned %>%
      mutate(
        caption = caption %>% str_to_sentence %>%
          paste0(., ", United States") %>%
          gsub("a1c", "A1c", .) %>%
          gsub("($)", "", ., fixed = T) %>%
          gsub("(%)", "", ., fixed = T) %>%
          gsub(" ,", ",", ., fixed = T) %>% str_squish %>%
          gsub(",,", ",", ., fixed = T) %>% str_squish 
      )
    

    # fmt_captioned %>%
    #   #filter(stat_var == "totPOP") %>%
    #   select(byGrps, caption) %>% distinct %>% print(n=100)


    # Final processing steps and output ---------------------------------------
    out_tbl <- fmt_captioned %>%
      
      rename(row_var = rowGrp, col_var = colGrp) %>%
      
      select(one_of(
        "Year",
        "stat_group", "stat_var", "stat_label",
        "row_group", "row_var", "row_label", "rowLevels",
        "col_group", "col_var", "col_label", "colLevels", 
        "adult_child", "value", "se", "asterisk", "Percent", "sample_size",
        "caption")) %>%
      
      mutate(
        stat_var = factor(stat_var, levels = stat_order),
        row_var  = factor(row_var, levels = grp_order),
        col_var  = factor(col_var, levels = grp_order)) %>%
      
      arrange(stat_var, row_var, col_var) %>%
      
      # Remove impossible combinations: insurance <65/65+ & age groups 65+/<65
      filter(
        !(col_var == "ins_lt65" & colLevels %>% startsWith("65+")),
        !(col_var == "ins_ge65" & colLevels %>% startsWith("<65"))
      )
    
    
    # QC: check that we don't have multiple values for row/col/stat
    qc_dups <- out_tbl %>% select(one_of("adult_child", "row_var", "rowLevels", "col_var", "colLevels", "Year", "stat_var"))
    out_tbl[duplicated(qc_dups)|duplicated(qc_dups, fromLast = T),] %>% print
 
    
    write_csv(out_tbl, file = str_glue("{fmt_dir}/DY{year}.csv"), na = "--")
    
    
  } # end for year in years
  
} # end format_tables function
