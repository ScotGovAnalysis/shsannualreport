#' Extract survey metadata from Excel
#'
#' \code{shs_extract_metadata} extracts survey metadata from Excel workbooks in a specified location,
#' and saves each sheet into a specified destination as an individual \code{.Rds} file.
#' Internal function for \code{shs_extract_data}.
#'
#' @param source_metadata_path \code{string}. The path of the directory containing survey metadata in Excel format.
#' @param extracted_metadata_path \code{string}. The path of the directory to be extracted to, created by \code{shs_extract_data}.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_extract_metadata(source_metadata_path, extracted_metadata_path)
#'
#' @keywords internal
#'
#' @noRd

shs_extract_metadata <- function(source_metadata_path,
                                 extracted_metadata_path) {

  # Get all metadata files
  files <- list.files(source_metadata_path)

  # Loop through metadata files
  for (file in files) {

    # Get sheets in file
    #TODO: Depends on two packages, as need to extract sheet names, refactor
    # workbook_path <- file.path(source_metadata_path, file)
    # workbook <- XLConnect::loadWorkbook(workbook_path)
    # sheets <- readxl::excel_sheets(workbook_path)
    ###
    workbook_path <- file.path(source_metadata_path, file)
    sheets <- readxl::excel_sheets(workbook_path)
    ###


    # Save sheet as .Rds file in output directory
    for (sheet in sheets) {
      # df <- XLConnect::readWorksheet(workbook, sheet = sheet, header = TRUE)
      df <- readxl::read_excel(workbook_path, sheet = sheet)
      saveRDS(df, file = file.path(extracted_metadata_path,
                                   paste0(sheet, ".Rds")))
    }
  }
}
