report_data_processing <- function(topic, local_authority, year, comparison_type, comparator) {

  data_list_string <- "data_list <- list("

  question_list <- question_titles[question_titles$Topic == topic,]$ID

  for (question in question_list) {

    table <- table_processing(question, local_authority, year, comparison_type, comparator)

    variable_column_names <- colnames(table)[2:length(colnames(table))]

    measure_column_name <- colnames(table)[1]

    variable_column_names <- variable_column_names[!grepl("_l", variable_column_names) & !grepl("_u", variable_column_names) & !grepl("_sig", variable_column_names) & !grepl("_2", variable_column_names) & !variable_column_names %in% c(measure_column_name, "Year", "Council", "All", "Base")]

    variable_column_names

    table <- eval(parse(text = round_string("table", variable_column_names)))

    assign(question, table)

    data_list_string <- paste0(data_list_string, "`", question, "`, ")
  }

  data_list_string <- paste0(substr(data_list_string, 1, nchar(data_list_string) - 2), ")")

  eval(parse(text = data_list_string))

  data_list
}
