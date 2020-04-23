report_data_processing <- function(topic, local_authority, year, comparison_type, comparator) {

  data_list_string <- "data_list <- list("

  question_list <- question_titles[question_titles$Topic == topic,]$ID

  for (question in question_list) {

    table <- readRDS(paste0("data/dataset/", question, ".Rds"))

    question_type <- question_titles[question_titles$ID == question,]$Type

    if (question_type %in% c("1", "4")) {

      row_variable <- colnames(table)[2]

      column_variables <- colnames(table)[!grepl("_l", colnames(table)) & !grepl("_u", colnames(table)) & !colnames(table) %in% c(row_variable, "Year", "Council", "All", "Base")]

      merge_by <- paste0("\"", row_variable, "\"")

    } else if(question_type %in% c("2", "3")) {

      row_variable <- colnames(table)[3]

      column_variables <- colnames(table)[!grepl("_l", colnames(table)) & !grepl("_u", colnames(table)) & !colnames(table) %in% c(row_variable, "Year", "Council")]

      if (comparison_type == "Local Authority") {

        merge_by <- paste0("c(\"Year\", \"", row_variable, "\")")

      } else if (comparison_type == "Year") {

        merge_by <- paste0("c(\"Council\", \"", row_variable, "\")")
      }
    }

    eval(parse(text = main_table_string(question_type = question_type)))

    if (comparison_type == "No comparison" | (question_type %in% c("1", "4") & comparison_type == "Year"))  {

      table <- table_main

    } else if ((question_type %in% c("2", "3") & comparison_type != "No comparison") | (question_type %in% c("1", "4") & !comparison_type %in% c("Year", "No comparison"))) {

      eval(parse(text = comparison_table_string(comparison_type = comparison_type, question_type = question_type, column_variables = column_variables)))

      eval(parse(text = merge_string(question_type = question_type, merge_by = merge_by, row_variable = row_variable, column_variables = column_variables)))

      eval(parse(text = remove_significance_string(row_variable = row_variable)))
    }

    eval(parse(text = arrange_select_mutate_string(comparison_type = comparison_type, question_type = question_type, row_variable = row_variable, column_variables = column_variables)))

    assign(question, table)

    data_list_string <- paste0(data_list_string, "`", question, "`, ")
  }

  data_list_string <- paste0(substr(data_list_string, 1, nchar(data_list_string) - 2), ")")

  eval(parse(text = data_list_string))

  data_list
}
