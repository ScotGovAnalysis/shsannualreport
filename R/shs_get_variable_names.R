#' Get variable names from an extracted dataset
#'
#' \code{shs_get_variable_names} gets variable names from extracted SHS survey data in a specified location,
#' and saves them to an xlsx file, which can then be updated with names to display the annual report Shiny app.
#'
#' @param extracted_dataset_path \code{string}. The path of the directory containing extracted survey data.
#' @param column_and_variable_path \code{string}. The path to save the variable names Excel workbook to.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_get_variable_names(extracted_dataset_path)
#' }
#'
#' @export

shs_get_variable_names <- function(extracted_data_path, column_and_variable_path) {

  extracted_dataset_path <- file.path(extracted_data_path, "dataset")
  files <- list.files(extracted_dataset_path)
  all_variable_names <- c()

  for (file in files) {

    file_path <- file.path(extracted_dataset_path, file)
    df <- readRDS(file_path)

    if ("sort" %in% colnames(df)){
      df <- subset(df, select=-c(sort))
    }

    if ("_LABEL_" %in% colnames(df)){
      df <- subset(df, select=-c(`_LABEL_`))
    }

    variable_names <- unique(df[2])[[1]]
    all_variable_names <- c(all_variable_names, variable_names)
  }

  all_variable_names <- unique(all_variable_names)
  all_variable_names <- data.frame(all_variable_names)
  colnames(all_variable_names)[1] <- "source_name"
  all_variable_names$display_name <- ""

  writexl::write_xlsx(list(variable_names = all_variable_names), path = file.path(column_and_variable_path, "variable_names.xlsx"))
}
