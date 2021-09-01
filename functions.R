
# Helper functions ------------------------------------------------------------
pop <- function(vec, ...) vec[!vec %in% unlist(list(...))]

p <- paste0

add_v2X <- function(names) names %>% append(c('agegrps_v2X', 'insurance_v2X'))
add_v3X <- function(names) names %>% append(c('agegrps_v2X', 'agegrps_v3X'))

is.odd  <- function(x) { x %% 2 == 1 }
is.even <- function(x) { x %% 2 == 0}

add_tot_sops <- function(df, ev, yr, sop_vars) {
  doc_sops <- str_glue("{ev}D{sop_vars}{yr}X") %>% sort
  fac_sops <- str_glue("{ev}F{sop_vars}{yr}X") %>% sort
  tot_sops <- str_glue("{ev}{sop_vars}{yr}X")  %>% sort %>% c
  
  df[, tot_sops] <- df[, doc_sops] + df[, fac_sops]
  return(df)
}

rm_evt_key <- function(df, ev, yr, sop_vars) {
  old_vars <- str_glue("{ev}{sop_vars}{yr}X") %>% sort
  new_vars <- str_glue("{sop_vars}{yr}X") %>% sort %>% c
  
  df[,new_vars] = df[,old_vars]
  return(df)
}

# Run functions ---------------------------------------------------------------

stdize <- function(df, row, stat) {
  
  # Convert from wide to long
  dfX <- df %>%
    gather(key = colLevels, value = stat, -row) %>%
    filter(!grepl("FALSE", colLevels)) %>%
    mutate(colLevels = gsub(" > 0TRUE","",colLevels))
  
  # Split coefs and ses, then re-join
  coefs <- dfX %>% filter(!grepl("se.",colLevels, fixed = T))
  ses <- dfX %>% filter(grepl("se.", colLevels, fixed = T)) %>%
    mutate(colLevels = gsub("se.","",colLevels, fixed = T)) %>%
    rename(se = stat)
  
  out <- full_join(coefs, ses)
  
  out %>% setNames(c("rowLevels", "colLevels", stat, paste0(stat, "_se")))
}

update.csv <- function(add, file, dir){
  add <- add %>%
    select(one_of("rowGrp", "colGrp", "rowLevels", "colLevels", colnames(.)))
  
  init = !(file %in% list.files(dir, recursive=T))
  fileName <- sprintf("%s/%s", dir, file) %>% gsub("//","/",.)
  write.table(add, file = fileName, append = (!init), sep = ",", 
              col.names = init, row.names = F)
}

# Format functions ------------------------------------------------------------

add_keys <- function(df, keys) {
  row_keys <- keys %>% rename(rowGrp = var, rowLevels = level, row_key = label)
  col_keys <- keys %>% rename(colGrp = var, colLevels = level, col_key = label)
  
  # Only adjust rowLevels/colLevels if they are already in data frame
  if("rowLevels" %in% colnames(df)) {
    df <- df %>%  
      left_join(row_keys) %>%
      mutate(rowLevels = ifelse(!is.na(row_key), row_key, rowLevels)) %>%
      select(-row_key)
  }
  
  if("colLevels" %in% colnames(df)) {
    df <- df %>%  
      left_join(col_keys) %>%
      mutate(colLevels = ifelse(!is.na(col_key), col_key, colLevels)) %>%
      select(-col_key)
  }
  
  return(df)
}

add_totals <- function(df, var = 'row') {
  
  df[,"var"] = df[,paste0(var,"Grp")]
  df[,"lev"] = df[,paste0(var,"Levels")]
  
  totals <- df %>% filter(var == "ind")
  all_grps <- df$var %>% unique %>% pop('ind')
  
  totals_list <- list()
  for(grp in all_grps %>% pop("sop")) {
    label = ifelse(grp == "event", "Any event", "All persons")
    totals_list[[grp]] <- totals %>% mutate(var = grp, lev = label)
  }
  all_totals <- bind_rows(totals_list)
  all_totals[,paste0(var,"Grp")] = all_totals$var
  all_totals[,paste0(var,"Levels")] = all_totals$lev
  
  out <- bind_rows(df, all_totals) %>% select(-var, -lev)
  
  # If 'Any event' is already calculated, remove the calculated version
  distinct_names <- out %>% select(-value, -se, -sample_size) %>% colnames
  distinct_out   <- out %>% distinct(across(all_of(distinct_names)), .keep_all = T)

  return(distinct_out)
}

rm_v2 <- function(df){
  df %>% 
    mutate(
      rowGrp = rowGrp %>% gsub("_v2X","",.) %>% gsub("_v3X","",.),
      colGrp = colGrp %>% gsub("_v2X","",.) %>% gsub("_v3X","",.))
}

switch_labels <- function(df){
  df %>%
    mutate(g1 = rowGrp, g2 = colGrp, l1 = rowLevels, l2 = colLevels) %>%
    mutate(rowGrp = g2, colGrp = g1, rowLevels = l2, colLevels = l1) %>%
    select(-g1, -g2, -l1, -l2)
}
