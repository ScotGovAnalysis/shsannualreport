#' Extract an SHS dataset from Excel workbooks
#'
#' \code{shs_extract_dataset} extracts raw survey data from Excel workbooks in a specified location,
#' and saves each sheet into a specified destination as an individual \code{.Rds} file.
#' Internal function for \code{shs_extract_data}.
#'
#' @param source_dataset_path \code{string}. The path of the directory containing survey data in Excel format.
#' @param extracted_dataset_path \code{string}. The path of the directory to be extracted to, created by \code{shs_extract_data}.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_extract_dataset(source_dataset_path, extracted_dataset_path)
#'
#' @keywords internal
#'
#' @noRd

shs_extract_dataset <- function(source_dataset_path, extracted_dataset_path) {

  # Get all dataset files
  files <- list.files(source_dataset_path)

  # Get year of dataset
  years <- list()

  for (file in files) {
    years <- c(years, sub(".*SHS *(.*?) *_CH.*", "\\1", file))
  }

  year <- unique(years)

  # Loop through dataset files
  for (file in files) {
    workbook_path <- file.path(source_dataset_path, file)

    #TODO: Depends on two packages, as need to extract sheet names, refactor
    workbook <- XLConnect::loadWorkbook(workbook_path)
    sheets <- readxl::excel_sheets(workbook_path)

    # Loop through sheets in file
    for (sheet in sheets) {

      # Check whether sheet is 'TAB' or 'FIG' and label accordingly
      if (grepl("TAB", sheet)) {
        chapter_number <- sub(".*FINAL_C *(.*?) *_TAB.*", "\\1", sheet)
        tab_number <- sub(".*_TAB *(.*?)", "\\1", sheet)
        dataframe_id <- paste0("Table ", chapter_number, ".", tab_number)
      }
      else if (grepl("FIG", sheet)) {
        chapter_number <- sub(".*FINAL_C *(.*?) *_FIG.*", "\\1", sheet)
        fig_number <- sub(".*_FIG *(.*?)", "\\1", sheet)
        dataframe_id <- paste0("Figure ", chapter_number, ".", fig_number)
      }
      else {
        stop(paste0("Unknown type of data in sheet ", sheet, ".
                    Only 'FIG' or 'TAB' sheets permitted."))
      }

      # Read worksheet to dataframe
      df <- XLConnect::readWorksheet(workbook, sheet = sheet, header = TRUE)

      # Save dataframe as .Rds file
      saveRDS(df, file = file.path(extracted_dataset_path,
                                   paste0(dataframe_id, ".Rds")))
    }
  }
}
