#' Extract survey column and variable_names from Excel
#'
#' \code{shs_extract_column_and_variable_names} extracts survey metadata from Excel workbooks in a specified location,
#' and saves each sheet into a specified destination as an individual \code{.Rds} file.
#'
#' @param source_column_and_variable_names_path \code{string}. The path of the directory containing survey column and variable_names in Excel format.
#' @param extracted_metadata_path \code{string}. The path of the directory to be extracted to, created by \code{shs_extract_data}.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_extract_column_and_variable_names(source_column_and_variable_names_path, extracted_metadata_path)
#' }
#'
#' @export

shs_extract_column_and_variable_names <- function(source_column_and_variable_names_path,
                                 extracted_metadata_path) {

  # Get all column and variable_names files
  files <- list.files(source_column_and_variable_names_path)

  # Loop through column and variable_names files
  for (file in files) {

    workbook_path <- file.path(source_column_and_variable_names_path, file)
    sheets <- readxl::excel_sheets(workbook_path)

    # Save sheet as .Rds file in output directory
    for (sheet in sheets) {

      df <- readxl::read_excel(workbook_path, sheet = sheet)
      saveRDS(df, file = file.path(extracted_metadata_path,
                                   paste0(sheet, ".Rds")))
    }
  }
}
