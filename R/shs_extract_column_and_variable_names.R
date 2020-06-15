#' Extract survey column and variable_names from Excel
#'
#' \code{shs_extract_column_and_variable_names} extracts survey metadata from Excel workbooks in a specified location,
#' and saves each sheet into a specified destination as an individual \code{.Rds} file.
#'
#' @param app_metadata_directory \code{string}. The path of the app directory containing metadata.
#' @param column_names_save_file_path \code{string}. The path of the Excel file containing column names metadata.
#' @param variable_names_save_file_path \code{string}. The path of the Excel file containing variable names metadata.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_extract_column_and_variable_names(app_metadata_directory, column_names_save_file_path, variable_names_save_file_path)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_extract_column_and_variable_names <- function(app_metadata_directory, column_names_save_file_path, variable_names_save_file_path) {

  files <- c(column_names_save_file_path, variable_names_save_file_path)

  for (file in files) {

    sheets <- readxl::excel_sheets(file)

    for (sheet in sheets) {

      df <- readxl::read_excel(file, sheet = sheet)
      saveRDS(df, file = file.path(app_metadata_directory, paste0(sheet, ".Rds")))
    }
  }
}
