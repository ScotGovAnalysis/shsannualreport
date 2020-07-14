#' Extract survey column and variable_names from Excel
#'
#' \code{shs_extract_column_and_variable_names} extracts survey metadata from Excel workbooks in a specified location,
#' and saves each sheet into a specified destination as an individual \code{.Rds} file.
#'
#' @param metadata_directory \code{string}.
#' The path of the app directory containing metadata.
#' @param column_names_workbook_path \code{string}.
#' The path of the Excel file containing column names metadata.
#' @param variable_names_workbook_path \code{string}.
#' The path of the Excel file containing variable names metadata.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_extract_column_and_variable_names(metadata_directory, column_names_workbook_path, variable_names_workbook_path)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_extract_column_and_variable_names <- function(metadata_directory,
                                                  column_names_workbook_path,
                                                  variable_names_workbook_path) {

  files <- c(column_names_workbook_path, variable_names_workbook_path)

  for (file in files) {

    sheets <- readxl::excel_sheets(file)

    for (sheet in sheets) {

      df <- readxl::read_excel(file, sheet = sheet)
      saveRDS(df, file = file.path(metadata_directory, paste0(sheet, ".Rds")))
    }
  }
}
