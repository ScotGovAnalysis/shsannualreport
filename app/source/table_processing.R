table_processing <- function(question, local_authority, year, comparison_type, comparator) {

  year_present <- TRUE

  comparison_year_present <- TRUE

  question_type <- question_titles[question_titles$ID == question,]$Type

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

      if (comparison_type == "Local Authority") {

        merge_by <- paste0("c(\"Year\", \"", row_variable, "\")")

      } else if (comparison_type == "Year" & comparison_year_present == TRUE) {

        merge_by <- paste0("c(\"Council\", \"", row_variable, "\")")

      }
    }
  }

  eval(parse(text = main_table_string(question_type = question_type, year_present = year_present)))

  if (comparison_type == "No comparison" | (question_type %in% c("1", "4") & comparison_type == "Year") | (question_type %in% c("2", "3") & comparison_year_present == FALSE))  {

    table <- table_main

  } else if ((question_type %in% c("2", "3") & comparison_type != "No comparison" & year_present == TRUE & comparison_year_present == TRUE) | (question_type %in% c("1", "4") & !comparison_type %in% c("Year", "No comparison"))) {

    eval(parse(text = comparison_table_string(comparison_type = comparison_type, question_type = question_type, column_variables = column_variables)))

    eval(parse(text = merge_string(question_type = question_type, merge_by = merge_by, row_variable = row_variable, column_variables = column_variables)))

    eval(parse(text = remove_significance_string(row_variable = row_variable)))
  }

  if (question_type == "0" | year_present == FALSE) {

    table <- NULL

  } else if (question_type %in% c("1", "2", "3") & year_present == TRUE) {

    eval(parse(text = arrange_row_variables_string(row_variable = row_variable)))

    eval(parse(text = round_string(column_variables = column_variables, comparison_year_present = comparison_year_present)))

    table <- table[colnames(table) != "Year" & !grepl("Council", colnames(table))]

  }  else if (question_type == "4") {

    table <- table[colnames(table) != "Year" & !grepl("Council", colnames(table))]

  }
}
