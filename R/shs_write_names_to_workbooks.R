#' Write column and variable names from a dataset to Excel
#'
#' \code{shs_write_names_to_workbooks} gets column and variable names from raw SHS survey data Excel format,
#' and saves them to xlsx files, which can then be updated with names to display in the annual report Shiny app.
#' The output file 'column_names.xlsx' will contain all column names in the source files, minus any specified in
#' the \code{column_names_to_exclude} argument.
#' 'variable_names.xlsx' will contain all unique values present in the second column of all tables in the raw data,
#' once columns names in \code{column_names_to_exclude} are removed.
#' The resulting Excel sheets will have two columns: source_name and display_name. The display name value is the value
#' that will be displayed in the final Shiny app, these values can be added manually or by using
#' \code{shs_update_names_workbook}
#'
#' @param source_dataset_directory \code{string}.
#' The path of the directory containing raw survey data in Excel format.
#' @param destination_directory \code{string}.
#' The path to save the output Excel files to.
#' @param columns_to_remove \code{list}.
#' Names of extraneous columns, to exclude from variable names processing.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_write_names_to_workbooks(source_dataset_directory, destination_directory, columns_to_remove)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_write_names_to_workbooks <- function(source_dataset_directory, destination_directory, columns_to_remove) {

  files <- list.files(source_dataset_directory)

  column_names_save_path <- file.path(destination_directory, "column_names.xlsx")
  variable_names_save_path <- file.path(destination_directory, "variable_names.xlsx")

  all_column_names <- c()
  all_variable_names <- c()

  for (file in files) {

    workbook_path <- file.path(source_dataset_directory, file)
    sheets <- readxl::excel_sheets(workbook_path)

    for (sheet in sheets) {

      df <- readxl::read_excel(workbook_path, sheet = sheet)

      column_names <- colnames(df)

      df <- df[, !(column_names %in% columns_to_remove)]

      column_names <- colnames(df)

      all_column_names <- c(all_column_names, column_names)

      variable_names <- unique(df[2])[[1]]
      all_variable_names <- c(all_variable_names, variable_names)
    }
  }

  all_column_names <- unique(all_column_names)
  all_column_names <- data.frame(all_column_names)
  colnames(all_column_names)[1] <- "source_name"
  all_column_names$display_name <- ""

  all_variable_names <- unique(all_variable_names)
  all_variable_names <- data.frame(all_variable_names)
  colnames(all_variable_names)[1] <- "source_name"
  all_variable_names$display_name <- ""

  writexl::write_xlsx(list(column_names = all_column_names), path = column_names_save_path)
  writexl::write_xlsx(list(variable_names = all_variable_names), path = variable_names_save_path)
}
