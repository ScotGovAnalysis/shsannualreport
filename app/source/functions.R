# FUNCTIONS ####

# The following functions dynamically create R strings based on the arguments provided, which are evaluated in app.R.
# This is to allow values (such as column names) to change based on user input.
# Unfortunately this makes the code less readable, so explanations and examples are provided for each function.
# As line breaks are output and evaluated as '\n' in R, the output strings cannot be formatted with line breaks.
# The functions are either identical or similar for time series and column percentage tables; differences are dealt with by if/elses.


# COMPARISON DF ####

# The comparison dataframe is the table produced when the user selects either "Year" or "Local Authority" in the "Compare by" input.
# Because statistically significant difference in the main dataframe is calculated based on the comparison df, it must be processed first.


# round_comparison_df_values example ####

# dplyr::mutate(df, 
#               `1999/2000` = round(`1999/2000`, 0), 
#               `2001/2002` = round(`2001/2002`, 0), 
#               `2003/2004` = round(`2003/2004`, 0), 
#               `2005/2006` = round(`2005/2006`, 0), 
#               `2007/2008` = round(`2007/2008`, 0), 
#               `2009/2010` = round(`2009/2010`, 0), 
#               `2011` = round(`2011`, 0), 
#               `2012` = round(`2012`, 0), 
#               `2013` = round(`2013`, 0), 
#               `2014` = round(`2014`, 0), 
#               `2015` = round(`2015`, 0), 
#               `2016` = round(`2016`, 0), 
#               `2017` = round(`2017`, 0), 
#               `2018` = round(`2018`, 0)) 

# round_comparison_df_values ####

round_comparison_df_values <- function(variable_column_names) {
  
  round_string = "dplyr::mutate(df,"
  
  for (variable_column_name in variable_column_names) {
    if (!grepl("_l", variable_column_name) & !grepl("_u", variable_column_name)) {
      round_string  = paste0(round_string , " `", variable_column_name, "` = ifelse(`", variable_column_name, "` > 0, suppressWarnings(as.character(round(as.numeric(`", variable_column_name, "`,  0)))), `", variable_column_name, "`),")
    }
  }
  round_string  <- (substr(round_string, 1, nchar(round_string ) - 1)) 
  round_string  <- paste0(round_string, ")")

  # print(paste0("round_string: ", round_string))
  return(round_string)
}

# rename_comparison_df_values example ####

# dplyr::rename(df,
#               `Council2` = `Council`,
#               `1999/20002` = `1999/2000`,
#               `2001/20022` = `2001/2002`,
#               `2003/20042` = `2003/2004`,
#               `2005/20062` = `2005/2006`,
#               `2007/20082` = `2007/2008`,
#               `2009/20102` = `2009/2010`,
#               `20112` = `2011`,
#               `20122` = `2012`,
#               `20132` = `2013`,
#               `20142` = `2014`,
#               `20152` = `2015`,
#               `20162` = `2016`,
#               `20172` = `2017`,
#               `20182` = `2018`,
#               `1999/2000_l2` = `1999/2000_l`,
#               `2001/2002_l2` = `2001/2002_l`,
#               `2003/2004_l2` = `2003/2004_l`,
#               `2005/2006_l2` = `2005/2006_l`,
#               `2007/2008_l2` = `2007/2008_l`,
#               `2009/2010_l2` = `2009/2010_l`,
#               `2011_l2` = `2011_l`,
#               `2012_l2` = `2012_l`,
#               `2013_l2` = `2013_l`,
#               `2014_l2` = `2014_l`,
#               `2015_l2` = `2015_l`,
#               `2016_l2` = `2016_l`,
#               `2017_l2` = `2017_l`,
#               `2018_l2` = `2018_l`,
#               `1999/2000_u2` = `1999/2000_u`,
#               `2001/2002_u2` = `2001/2002_u`,
#               `2003/2004_u2` = `2003/2004_u`,
#               `2005/2006_u2` = `2005/2006_u`,
#               `2007/2008_u2` = `2007/2008_u`,
#               `2009/2010_u2` = `2009/2010_u`,
#               `2011_u2` = `2011_u`,
#               `2012_u2` = `2012_u`,
#               `2013_u2` = `2013_u`,
#               `2014_u2` = `2014_u`,
#               `2015_u2` = `2015_u`,
#               `2016_u2` = `2016_u`,
#               `2017_u2` = `2017_u`,
#               `2018_u2` = `2018_u`)

# rename_comparison_df_values ####

rename_comparison_df_columns <- function(column_names, measure_column_name) {
  
  rename_string = "dplyr::rename(df,"
  
  for (column_name in column_names) {
    if (column_name != measure_column_name) {
      rename_string <- paste0(rename_string, "`", column_name, "2` = `", column_name, "`,")
    }
  }
  rename_string <- (substr(rename_string, 1, nchar(rename_string) - 1)) 
  rename_string <- paste0(rename_string, ")")

  # print(paste0("rename_string: ", rename_string))
  return(rename_string)
}

# MAIN DF #####


# statistical_significance example ####

# dplyr::mutate(main_df,
#               `1999/2000_sig`= case_when(`1999/2000_l` > `1999/2000_u2` ~ 'HIGHER', `1999/2000_u` < `1999/2000_l2` ~ 'LOWER', TRUE ~ 'NO'),
#               `2001/2002_sig`= case_when(`2001/2002_l` > `2001/2002_u2` ~ 'HIGHER', `2001/2002_u` < `2001/2002_l2` ~ 'LOWER', TRUE ~ 'NO'),
#               `2003/2004_sig`= case_when(`2003/2004_l` > `2003/2004_u2` ~ 'HIGHER', `2003/2004_u` < `2003/2004_l2` ~ 'LOWER', TRUE ~ 'NO'),
#               `2005/2006_sig`= case_when(`2005/2006_l` > `2005/2006_u2` ~ 'HIGHER', `2005/2006_u` < `2005/2006_l2` ~ 'LOWER', TRUE ~ 'NO'),
#               `2007/2008_sig`= case_when(`2007/2008_l` > `2007/2008_u2` ~ 'HIGHER', `2007/2008_u` < `2007/2008_l2` ~ 'LOWER', TRUE ~ 'NO'),
#               `2009/2010_sig`= case_when(`2009/2010_l` > `2009/2010_u2` ~ 'HIGHER', `2009/2010_u` < `2009/2010_l2` ~ 'LOWER', TRUE ~ 'NO'),
#               `2011_sig`= case_when(`2011_l` > `2011_u2` ~ 'HIGHER', `2011_u` < `2011_l2` ~ 'LOWER', TRUE ~ 'NO'),
#               `2012_sig`= case_when(`2012_l` > `2012_u2` ~ 'HIGHER', `2012_u` < `2012_l2` ~ 'LOWER', TRUE ~ 'NO'),
#               `2013_sig`= case_when(`2013_l` > `2013_u2` ~ 'HIGHER', `2013_u` < `2013_l2` ~ 'LOWER', TRUE ~ 'NO'),
#               `2014_sig`= case_when(`2014_l` > `2014_u2` ~ 'HIGHER', `2014_u` < `2014_l2` ~ 'LOWER', TRUE ~ 'NO'),
#               `2015_sig`= case_when(`2015_l` > `2015_u2` ~ 'HIGHER', `2015_u` < `2015_l2` ~ 'LOWER', TRUE ~ 'NO'),
#               `2016_sig`= case_when(`2016_l` > `2016_u2` ~ 'HIGHER', `2016_u` < `2016_l2` ~ 'LOWER', TRUE ~ 'NO'),
#               `2017_sig`= case_when(`2017_l` > `2017_u2` ~ 'HIGHER', `2017_u` < `2017_l2` ~ 'LOWER', TRUE ~ 'NO'),
#               `2018_sig`= case_when(`2018_l` > `2018_u2` ~ 'HIGHER', `2018_u` < `2018_l2` ~ 'LOWER', TRUE ~ 'NO'))

# statistical_significance ####

statistical_significance <- function(variable_column_names) {
  
  statistical_significance_string <- "dplyr::mutate(base_df,"
  
  for (variable_column_name in variable_column_names) {
    if (!grepl("_l", variable_column_name) & !grepl("_u", variable_column_name)) {
      statistical_significance_string <- paste0(statistical_significance_string, 
                                                "`", variable_column_name, "_sig`= case_when(`", variable_column_name, "_l` > `", variable_column_name, "_u2` ~ 'HIGHER', `",
                                                variable_column_name, "_u` < `", variable_column_name, "_l2` ~ 'LOWER', TRUE ~ 'NO'),"
      )
    }
  }
  statistical_significance_string <- (substr(statistical_significance_string, 1, nchar(statistical_significance_string) - 1)) 
  statistical_significance_string <- paste0(statistical_significance_string, ")")
  
  # print(paste0("statistical_significance_string: ", statistical_significance_string))
  return(statistical_significance_string)
}

remove_significance_from_rows <- function(measure_column_name) {
  remove_significance_from_rows_string <- paste0("base_df[base_df$`", measure_column_name, "` == 'All' | base_df$`", measure_column_name, "` == 'Base', colnames(base_df)[grep('_sig', colnames(base_df))]] <- \"NO\"")

  # print(remove_significance_from_rows_string)
return(remove_significance_from_rows_string)
}

# s_select_mutate example ####

# arrange(main_df, `Age`) %>% 
#   select(`Age`,
#          `1999/2000`,
#          `2001/2002`,
#          `2003/2004`,
#          `2005/2006`,
#          `2007/2008`,
#          `2009/2010`,
#          `2011`,
#          `2012`,
#          `2013`,
#          `2014`,
#          `2015`,
#          `2016`,
#          `2017`,
#          `2018`) %>% 
#   mutate(`1999/2000` = round(`1999/2000`, 0),
#          `2001/2002` = round(`2001/2002`, 0),
#          `2003/2004` = round(`2003/2004`, 0),
#          `2005/2006` = round(`2005/2006`, 0),
#          `2007/2008` = round(`2007/2008`, 0),
#          `2009/2010` = round(`2009/2010`, 0),
#          `2011` = round(`2011`, 0),
#          `2012` = round(`2012`, 0),
#          `2013` = round(`2013`, 0),
#          `2014` = round(`2014`, 0),
#          `2015` = round(`2015`, 0),
#          `2016` = round(`2016`, 0),
#          `2017` = round(`2017`, 0),
#          `2018` = round(`2018`, 0))

# arrange_select_mutate ####

arrange_select_mutate <- function(variable_column_names, measure_column_name) {
  
  arrange_select_mutate_string <- paste0("arrange(base_df(), `", measure_column_name, "`) %>% select(`", measure_column_name, "`,")
  
  for (variable_column_name in variable_column_names) {
    if (!grepl("_l", variable_column_name) & !grepl("_u", variable_column_name)) {
      arrange_select_mutate_string <- paste0(arrange_select_mutate_string, "`", variable_column_name, "`,")
    }
  }
  
  arrange_select_mutate_string <- (substr(arrange_select_mutate_string, 1, nchar(arrange_select_mutate_string) - 1)) 
  arrange_select_mutate_string <- paste0(arrange_select_mutate_string, ") %>% mutate(")
  
  for (variable_column_name in variable_column_names) {
    if (!grepl("_l", variable_column_name) & !grepl("_u", variable_column_name)) {
      arrange_select_mutate_string  = paste0(arrange_select_mutate_string, " `", variable_column_name, "` = ifelse(`", variable_column_name, "` > 0, suppressWarnings(as.character(round(as.numeric(`", variable_column_name, "`,  0)))), `", variable_column_name, "`),")
    }
  }
  
  arrange_select_mutate_string <- (substr(arrange_select_mutate_string, 1, nchar(arrange_select_mutate_string) - 1)) 
  arrange_select_mutate_string <- paste0(arrange_select_mutate_string, ")")
  
  # print(paste0("arrange_select_mutate_string: ", arrange_select_mutate_string))
  return(arrange_select_mutate_string)
}

# arrange_select_mutate_comparison example ####

# dplyr::arrange(main_df, `Age`) %>% 
#   select(`Age`,
#          `1999/2000`,
#          `2001/2002`,
#          `2003/2004`,
#          `2005/2006`,
#          `2007/2008`,
#          `2009/2010`,
#          `2011`,
#          `2012`,
#          `2013`,
#          `2014`,
#          `2015`,
#          `2016`,
#          `2017`,
#          `2018`,
#          `1999/2000_sig`,
#          `2001/2002_sig`,
#          `2003/2004_sig`,
#          `2005/2006_sig`,
#          `2007/2008_sig`,
#          `2009/2010_sig`,
#          `2011_sig`,
#          `2012_sig`,
#          `2013_sig`,
#          `2014_sig`,
#          `2015_sig`,
#          `2016_sig`,
#          `2017_sig`,
#          `2018_sig`) %>% 
#   mutate(`1999/2000` = round(`1999/2000`, 0),
#          `2001/2002` = round(`2001/2002`, 0),
#          `2003/2004` = round(`2003/2004`, 0),
#          `2005/2006` = round(`2005/2006`, 0),
#          `2007/2008` = round(`2007/2008`, 0),
#          `2009/2010` = round(`2009/2010`, 0),
#          `2011` = round(`2011`, 0),
#          `2012` = round(`2012`, 0),
#          `2013` = round(`2013`, 0),
#          `2014` = round(`2014`, 0),
#          `2015` = round(`2015`, 0),
#          `2016` = round(`2016`, 0),
#          `2017` = round(`2017`, 0),
#          `2018` = round(`2018`, 0))

# arrange_select_mutate_comparison ####

arrange_select_mutate_comparison <- function(variable_column_names, measure_column_name) {
  
  arrange_select_mutate_comparison_string <- paste0("dplyr::arrange(base_df(), `", measure_column_name, "`) %>% select(`", measure_column_name, "`,")
  
  for (variable_column_name in variable_column_names) {
    if (!grepl("_l", variable_column_name) & !grepl("_u", variable_column_name)) {
      arrange_select_mutate_comparison_string <- paste0(arrange_select_mutate_comparison_string, "`", variable_column_name, "`,")
    }
  }
  
  for (variable_column_name in variable_column_names) {
    if (!grepl("_l", variable_column_name) & !grepl("_u", variable_column_name) & variable_column_name != "Base" & variable_column_name != "All") {
      arrange_select_mutate_comparison_string <- paste0(arrange_select_mutate_comparison_string, "`", variable_column_name, "_sig`,")
    }
  }
  
  arrange_select_mutate_comparison_string <- (substr(arrange_select_mutate_comparison_string, 1, nchar(arrange_select_mutate_comparison_string) - 1)) 
  arrange_select_mutate_comparison_string <- paste0(arrange_select_mutate_comparison_string, ") %>% mutate(")
  
  for (variable_column_name in variable_column_names) {
    if (!grepl("_l", variable_column_name) & !grepl("_u", variable_column_name) & variable_column_name != "Base" & variable_column_name != "All") {
      arrange_select_mutate_comparison_string  = paste0(arrange_select_mutate_comparison_string, " `", variable_column_name, "` = ifelse(`", variable_column_name, "` > 0, suppressWarnings(as.character(round(as.numeric(`", variable_column_name, "`,  0)))), `", variable_column_name, "`),")
    }
  }
  
  arrange_select_mutate_comparison_string <- (substr(arrange_select_mutate_comparison_string, 1, nchar(arrange_select_mutate_comparison_string) - 1)) 
  arrange_select_mutate_comparison_string <- paste0(arrange_select_mutate_comparison_string, ")")
  
  # print(paste0("arrange_select_mutate_comparison_string: ", arrange_select_mutate_comparison_string))
  return(arrange_select_mutate_comparison_string)
}

# OUTPUTS #####

# main_df_comparison_output example ####

# DT::datatable(main_df(), 
#               options = list(digits = 1, 
#                              na = '-', 
#                              paging = FALSE, 
#                              ordering = FALSE, 
#                              info = FALSE, 
#                              searching = FALSE, 
#                              columnDefs = list(list(targets = c(0, 16:29), 
#                                                     visible = FALSE)))) %>% 
#   formatStyle(c('1999/2000',
#                 '2001/2002',
#                 '2003/2004',
#                 '2005/2006',
#                 '2007/2008',
#                 '2009/2010',
#                 '2011',
#                 '2012',
#                 '2013',
#                 '2014',
#                 '2015',
#                 '2016',
#                 '2017',
#                 '2018'), 
#               c('1999/2000_sig',
#                 '2001/2002_sig',
#                 '2003/2004_sig',
#                 '2005/2006_sig',
#                 '2007/2008_sig',
#                 '2009/2010_sig',
#                 '2011_sig',
#                 '2012_sig',
#                 '2013_sig',
#                 '2014_sig',
#                 '2015_sig',
#                 '2016_sig',
#                 '2017_sig',
#                 '2018_sig'), 
#               backgroundColor = styleEqual(c('NO', 'HIGHER', 'LOWER'),
#                                            c('transparent', '#00A3A3', '#C3C3FF')))

# main_df_comparison_output ####

main_df_comparison_output <- function(variable_column_names, hide_columns) {
  
  comparison_output_string <- paste0("DT::datatable(main_df(), options = list(digits = 1, na = '-', paging = FALSE, ordering = FALSE, info = FALSE, searching = FALSE, columnDefs = list(list(targets = c(0, ", hide_columns, "), visible = FALSE)))) %>% formatStyle(c(")
  
  variable_column_names_without_all_base <- variable_column_names[variable_column_names != "All" & variable_column_names != "Base"]
  
  for (variable_column_name in variable_column_names_without_all_base) {
    comparison_output_string <- paste0(comparison_output_string, "'", variable_column_name, "',")
  }
  
  comparison_output_string <- (substr(comparison_output_string, 1, nchar(comparison_output_string) - 1)) 
  comparison_output_string <- paste0(comparison_output_string, "), c(")
  
  for (variable_column_name in variable_column_names_without_all_base ) {
    comparison_output_string <- paste0(comparison_output_string, "'", variable_column_name, "_sig',")
  }
  
  comparison_output_string <- (substr(comparison_output_string, 1, nchar(comparison_output_string) - 1)) 
  comparison_output_string <- paste0(comparison_output_string, "), backgroundColor = styleEqual(c('NO', 'HIGHER', 'LOWER'),c('transparent', '#00A3A3', '#C3C3FF')))")
  
  # print(paste0("comparison_output_string: ", comparison_output_string))
  return(comparison_output_string)
}

# comparison_df_output example ####

# DT::datatable(comparison_df(), 
#               colnames = c('', 
#                            '', 
#                            'Age', 
#                            '1999/2000',
#                            '2001/2002',
#                            '2003/2004',
#                            '2005/2006',
#                            '2007/2008',
#                            '2009/2010',
#                            '2011',
#                            '2012',
#                            '2013',
#                            '2014',
#                            '2015',
#                            '2016',
#                            '2017',
#                            '2018'), 
#               options = list(digits = 1, 
#                              na = '-', 
#                              paging = FALSE, 
#                              ordering = FALSE, 
#                              info = FALSE, 
#                              searching = FALSE, 
#                              columnDefs = list(list(targets = c(0:1, 17:44), 
#                                                     visible = FALSE))))

# comparison_df_output ####

comparison_df_output <- function(measure_column_name, variable_column_names, hide_columns, target_end) {
  
  comparison_output_string <- "DT::datatable(comparison_df(), colnames = c('', '', "
  
  if (target_end == "2") {
    comparison_output_string <- paste0(comparison_output_string, "'', ")
  }
  
  comparison_output_string <- paste0(comparison_output_string, "'", measure_column_name, "', ")
  
  for (variable_column_name in variable_column_names) {
    comparison_output_string <- paste0(comparison_output_string, "'", variable_column_name, "',")
  }
  
  comparison_output_string <- (substr(comparison_output_string, 1, nchar(comparison_output_string) - 1)) 
  comparison_output_string <- paste0(comparison_output_string,
                                     "), options = list(digits = 1, na = '-', paging = FALSE, ordering = FALSE, info = FALSE, searching = FALSE, columnDefs = list(list(targets = c(0:",
                                     target_end,
                                     ", ",
                                     hide_columns,
                                     "), visible = FALSE))))")
  
  # print(paste0("comparison_output_string: ", comparison_output_string))
  return(comparison_output_string)
}

# chart_data_processing

chart_data_processing <- function(variable_column_names, measure_column_name, df_name) {
  
  chart_data_processing_string <- paste0(df_name, " %>% reshape(v.names = c(\"Percent\", \"LowerConfidenceLimit\", \"UpperConfidenceLimit\"), idvar = \"ID\", direction = \"long\", times = c(")
    
  for (variable_column_name in variable_column_names) {
    
    chart_data_processing_string <- paste0(chart_data_processing_string, "\"", variable_column_name, "\", ")
    
  }
    
  chart_data_processing_string <- (substr(chart_data_processing_string, 1, nchar(chart_data_processing_string) - 2))
  
  chart_data_processing_string <- paste0(chart_data_processing_string, "), varying = list(Percent = c(")
  
  for (variable_column_name in variable_column_names) {
    
    chart_data_processing_string <- paste0(chart_data_processing_string, "\"", variable_column_name, "\", ")
    
  }
  
  chart_data_processing_string <- (substr(chart_data_processing_string, 1, nchar(chart_data_processing_string) - 2))
  
  chart_data_processing_string <- paste0(chart_data_processing_string, "), LowerConfidenceLimit = c(")
  
  for (variable_column_name in variable_column_names) {
    
    chart_data_processing_string <- paste0(chart_data_processing_string, "\"", variable_column_name, "_l\", ")
    
  }
  
  chart_data_processing_string <- (substr(chart_data_processing_string, 1, nchar(chart_data_processing_string) - 2))
  
  chart_data_processing_string <- paste0(chart_data_processing_string, "), UpperConfidenceLimit = c(")
  
  for (variable_column_name in variable_column_names) {
    
    chart_data_processing_string <- paste0(chart_data_processing_string, "\"", variable_column_name, "_u\", ")
    
  }
  
  chart_data_processing_string <- (substr(chart_data_processing_string, 1, nchar(chart_data_processing_string) - 2))
  
  chart_data_processing_string <- paste0(chart_data_processing_string, 
                                         "))) %>% dplyr::select(`", measure_column_name, "`, `time`, `Percent`, `LowerConfidenceLimit`, `UpperConfidenceLimit`) %>%",
                                         "dplyr::mutate(`Percent`= as.numeric(`Percent`), `LowerConfidenceLimit`= as.numeric(`LowerConfidenceLimit`), `UpperConfidenceLimit`= as.numeric(`UpperConfidenceLimit`))")

  # print(chart_data_processing_string)
  return(chart_data_processing_string)
}
