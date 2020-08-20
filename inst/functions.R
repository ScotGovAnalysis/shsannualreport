# measure_column_name ####
measure_column_name <- function(df) {

  measure_column_name <- colnames(df)[1]

  measure_column_name
}

# variable_column_names ####
variable_column_names <- function(df, start_position) {

  variable_column_names <- colnames(df)[start_position:length(colnames(df))]

  variable_column_names <- variable_column_names[!grepl("_l", variable_column_names) &
                                                   !grepl("_u", variable_column_names) &
                                                   !grepl("_sig", variable_column_names) &
                                                   !variable_column_names %in% c(measure_column_name, "Year", "Council")]

  variable_column_names
}

# main_table_string ####
main_table_string <- function(question_type, year_present, local_authority_present) {

  if (question_type == "0" | year_present == FALSE | local_authority_present == FALSE) {

    main_table_string <- "table_main <- NULL"

  } else if (question_type %in% c("1", "4")) {

    main_table_string <- "table_main <- table[table$Council == local_authority,]"

  } else if(question_type %in% c("2", "3") & year_present == TRUE) {

    main_table_string <- "table_main <- table[table$Council == local_authority & table$Year == year,]"
  }

  # print(paste0("main_table_string : ", main_table_string))
  main_table_string
}

# comparison_table_string ####
comparison_table_string <- function(comparison_type, question_type, column_variables) {

  if (question_type %in% c("1", "4")) {

    comparison_table_string <- "table_comparison <- table[table$Council == comparator,] %>% dplyr::rename(`Council_2` = `Council`, "

  } else if(question_type %in% c("2", "3")) {

    if (comparison_type == "Local Authority/Scotland") {

      comparison_table_string <- "table_comparison <- table[table$Council == comparator & table$Year == year,] %>% dplyr::rename(`Council_2` = `Council`, "

    } else if (comparison_type == "Year") {

      comparison_table_string <- "table_comparison <- table[table$Council == local_authority & table$Year == comparator,] %>% dplyr::rename(`Year_2` = `Year`, "
    }
  }

  for (column_variable in column_variables) {

    addition_string <- paste0("`", column_variable, "_2` = `", column_variable, "`, ")

    comparison_table_string <- paste0(comparison_table_string, addition_string)
  }

  if (question_type %in% c("1", "2", "3")) {

    for (column_variable in column_variables) {

      if (!column_variable %in% c("All", "Base")) {

        addition_string <- paste0("`", column_variable, "_l_2` = `", column_variable, "_l`, ")

        comparison_table_string <- paste0(comparison_table_string, addition_string)
      }
    }

    for (column_variable in column_variables) {

      if (!column_variable %in% c("All", "Base")) {

        addition_string <- paste0("`", column_variable, "_u_2` = `", column_variable, "_u`, ")

        comparison_table_string <- paste0(comparison_table_string, addition_string)
      }
    }
  }

  comparison_table_string <- paste0(substr(comparison_table_string, 1, nchar(comparison_table_string) - 2), ")")

  # print(paste0("comparison_table_string : ", comparison_table_string))
  comparison_table_string
}

# merge_string ####
merge_string <- function(question_type, merge_by, row_variable, column_variables) {

  if (question_type %in% c("1", "2", "3")) {

    merge_string <- paste0("table <- merge(table_main, table_comparison, by = ", merge_by, ") %>%
    dplyr::mutate(")

    for (column_variable in column_variables) {

      if (!column_variable %in% c("All", "Base")) {

        addition_string <- paste0("`", column_variable, "_sig`= dplyr::case_when(
    (`", column_variable, "_l` > `", column_variable, "_u_2` |
       (as.numeric(`", column_variable, "`) > as.numeric(`", column_variable, "_2`)) &
          as.numeric(`", column_variable, "`) - as.numeric(`", column_variable, "_2`) >
          sqrt((as.numeric(`", column_variable, "`) - as.numeric(`", column_variable, "_l`))^2 + (as.numeric(`", column_variable, "_2`) - as.numeric(`", column_variable, "_l_2`))^2)) ~ 'HIGHER',
    (`", column_variable, "_u` < `", column_variable, "_l_2` |
       (as.numeric(`", column_variable, "_2`) > as.numeric(`", column_variable, "`)) &
       as.numeric(`", column_variable, "_2`) - as.numeric(`", column_variable, "`) >
       sqrt((as.numeric(`", column_variable, "`) - as.numeric(`", column_variable, "_l`))^2 + (as.numeric(`", column_variable, "_2`) - as.numeric(`", column_variable, "_l_2`))^2)) ~ 'LOWER',
    TRUE ~ 'NO'),
")

        merge_string <- paste0(merge_string, addition_string)

      }
    }

    merge_string <- paste0(substr(merge_string, 1, nchar(merge_string) - 2), ")")

  } else if (question_type == "4") {

    merge_string <- paste0("table <- plyr::join(table_main, table_comparison, by = ", merge_by, ")")
  }

  # print(paste0("merge_string: ", merge_string))
  merge_string
}

# remove_significance_string ####
remove_significance_string <- function(row_variable) {

  paste0("table[table$`", row_variable, "` == 'All' | table$`", row_variable, "` == 'Base', colnames(table)[grep('_sig', colnames(table))]] <- \"NO\"")
}

# arrange_row_variables_string ####
arrange_row_variables_string <- function(row_variable) {

  paste0("table <-  dplyr::arrange(table, `", row_variable, "`)")
}


# round_string ####
round_string <- function(table_name, column_variables, decimal_place) {

  round_string <- paste0("if (length(colnames(", table_name, ")[grep(\"_2\", colnames(", table_name, "))]) > 0) {
      ", table_name, " <- dplyr::mutate(", table_name, ", ")

  for (column_variable in column_variables) {

    addition_string <- paste0("`", column_variable, "` = ifelse(`", column_variable, "` > 0, suppressWarnings(as.character(janitor::round_half_up(as.numeric(`", column_variable, "`), digits = ", decimal_place, "))), `", column_variable, "`), ")

    round_string <- paste0(round_string, addition_string)
  }

  for (column_variable in column_variables) {

    addition_string <- paste0("`", column_variable, "_2` = ifelse(`", column_variable, "_2` > 0, suppressWarnings(as.character(janitor::round_half_up(as.numeric(`", column_variable, "_2`), digits = ", decimal_place, "))), `", column_variable, "_2`), ")

    round_string <- paste0(round_string, addition_string)
  }

  round_string <- paste0(substr(round_string, 1, nchar(round_string) - 2), ")
                           ",
                         "} else {
                           ", table_name, " <- dplyr::mutate(", table_name, ", ")

  for (column_variable in column_variables) {

    addition_string <- paste0("`", column_variable, "` = ifelse(`", column_variable, "` > 0, suppressWarnings(as.character(janitor::round_half_up(as.numeric(`", column_variable, "`),  digits = ", decimal_place, "))), `", column_variable, "`), ")

    round_string <- paste0(round_string, addition_string)

  }
  round_string <- paste0(substr(round_string, 1, nchar(round_string) - 2), ")
                           }")

  # print(paste0("round_string: ", round_string))
  round_string
}

# data_table_string ####
data_table_string <- function(df_name, variable_column_names, hide_columns, main_table) {

  data_table_string <- paste0("DT::datatable(", df_name, ", colnames = gsub(\"blank\", \"\", colnames(", df_name, ")), options = list(digits = 1, na = '-', paging = FALSE, ordering = FALSE, info = FALSE, searching = FALSE, columnDefs = list(list(targets = c(0, ", hide_columns, "), visible = FALSE), list(className = 'dt-right', targets = 2:ncol(table_df))))) %>% formatStyle(c(")

  variable_column_names_without_all_base <- variable_column_names[variable_column_names != "All" & variable_column_names != "Base"]

  for (variable_column_name in variable_column_names_without_all_base) {

    data_table_string <- paste0(data_table_string, "'", variable_column_name, "',")
  }

  data_table_string <- (substr(data_table_string, 1, nchar(data_table_string) - 1))

  data_table_string <- paste0(data_table_string, "), c(")

  for (variable_column_name in variable_column_names_without_all_base ) {

    data_table_string <- paste0(data_table_string, "'", variable_column_name, "_sig',")
  }

  data_table_string <- (substr(data_table_string, 1, nchar(data_table_string) - 1))

  if (main_table == TRUE) {

    data_table_string <- paste0(data_table_string, "), backgroundColor = styleEqual(c('NO', 'HIGHER', 'LOWER'),c('transparent', '#00A3A3', '#C3C3FF')))")

  } else if (main_table == FALSE) {

    data_table_string <- paste0(data_table_string, "), backgroundColor = styleEqual(c('NO', 'HIGHER', 'LOWER'),c('transparent', '#C3C3FF', '#00A3A3')))")
  }

  # print(paste0("data_table_string: ", data_table_string))
  data_table_string
}

# chart_data_processing_string ####
chart_data_processing_string <- function(variable_column_names, measure_column_name, df_name) {

  chart_data_processing_string <- paste0("as.data.frame(", df_name, ") %>% stats::reshape(v.names = c(\"Percent\", \"LowerConfidenceLimit\", \"UpperConfidenceLimit\"), idvar = \"ID\", direction = \"long\", times = c(")

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
  chart_data_processing_string
}

# table_processing ####
table_processing <- function(question, local_authority, year, comparison_type, comparator) {

  year_present <- TRUE

  comparison_year_present <- TRUE

  local_authority_present <- TRUE

  local_authority_comparison_present <- TRUE

  question_type <- question_titles[question_titles$ID == question,]$Type

  scotland_only <- question_titles[question_titles$ID == question,]$ScotlandOnly

  if (is.na(scotland_only)) {scotland_only <- "N"}

  if (scotland_only == "Y") {

    if (local_authority == "Scotland") {

      local_authority_present <- TRUE
      local_authority_comparison_present <- FALSE

    } else {

      local_authority_present <- FALSE
      local_authority_comparison_present <- FALSE
    }

  }

  if (question_type != "0") {

    table <- readRDS(paste0("data/dataset/", question, ".Rds"))
  }

  if (question_type %in% c("1", "4")) {

    row_variable <- colnames(table)[2]

    column_variables <- colnames(table)[!grepl("_l", colnames(table)) & !grepl("_u", colnames(table)) & !colnames(table) %in% c(row_variable, "Year", "Council", "All", "Base")]

    merge_by <- paste0("\"", row_variable, "\"")

  } else if(question_type %in% c("2", "3")) {

    if (length(table$Year[table$Year == year]) == 0) {

      year_present <- FALSE
    }

    if (comparison_type == "Year" & length(table$Year[table$Year == comparator]) == 0) {

      comparison_year_present <- FALSE
    }

    if (year_present == TRUE) {

      row_variable <- colnames(table)[3]

      column_variables <- colnames(table)[!grepl("_l", colnames(table)) & !grepl("_u", colnames(table)) & !colnames(table) %in% c(row_variable, "Year", "Council")]

      if (comparison_type == "Local Authority/Scotland") {

        merge_by <- paste0("c(\"Year\", \"", row_variable, "\")")

      } else if (comparison_type == "Year" & comparison_year_present == TRUE) {

        merge_by <- paste0("c(\"Council\", \"", row_variable, "\")")
      }
    }
  }

  eval(parse(text = main_table_string(question_type = question_type, year_present = year_present, local_authority_present = local_authority_present)))

  if (comparison_type == "No comparison" | (question_type %in% c("1", "4") & comparison_type == "Year") | (question_type %in% c("2", "3") & comparison_year_present == FALSE) | local_authority_comparison_present == FALSE)  {

    table <- table_main

  } else if ((question_type %in% c("2", "3") & comparison_type != "No comparison" & year_present == TRUE & comparison_year_present == TRUE) | (question_type %in% c("1", "4") & !comparison_type %in% c("Year", "No comparison"))) {

    eval(parse(text = comparison_table_string(comparison_type = comparison_type, question_type = question_type, column_variables = column_variables)))

    eval(parse(text = merge_string(question_type = question_type, merge_by = merge_by, row_variable = row_variable, column_variables = column_variables)))

    eval(parse(text = remove_significance_string(row_variable = row_variable)))
  }

  if (question_type == "0" | year_present == FALSE) {

    table <- NULL

  } else if (question_type %in% c("1", "2", "3") & year_present == TRUE & local_authority_present == TRUE ) {

    eval(parse(text = arrange_row_variables_string(row_variable = row_variable)))

    table <- table[!grepl("Year", colnames(table)) & !grepl("Council", colnames(table))]

  }  else if (question_type == "4") {

    table <- table[!grepl("Year", colnames(table)) & !grepl("Council", colnames(table))]
  }

  if (!is.null(table)) {

    if (nrow(table) == 0) {

      table <- NULL
    }
  }

  table
}

# report_data_processing ####
report_data_processing <- function(topic, local_authority, year, comparison_type, comparator) {

  report_data_string <- "report_data <- list("

  question_list <- question_titles[question_titles$Topic == topic,]$ID

  for (question in question_list) {

    table <- table_processing(question, local_authority, year, comparison_type, comparator)

    if (!is.null(table)) {

      variable_column_names <- colnames(table)[2:length(colnames(table))]

      measure_column_name <- colnames(table)[1]

      variable_column_names <- variable_column_names[!grepl("_l", variable_column_names) & !grepl("_u", variable_column_names) & !grepl("_sig", variable_column_names) & !grepl("_2", variable_column_names) & !variable_column_names %in% c(measure_column_name, "Year", "Council", "All", "Base")]

      variable_column_names

      table <- eval(parse(text = round_string("table", variable_column_names, 0)))

      table <- table[!grepl("_l", colnames(table)) & !grepl("_u", colnames(table))]

    }

    assign(question, table)

    report_data_string <- paste0(report_data_string, "`", question, "`, ")
  }

  report_data_string <- paste0(substr(report_data_string, 1, nchar(report_data_string) - 2), ")")

  eval(parse(text = report_data_string))

  report_data
}
