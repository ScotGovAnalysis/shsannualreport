#' Extract an SHS dataset from Excel workbooks
#'
#' \code{shs_extract_dataset} extracts raw survey data from Excel workbooks in a specified location,
#' and saves all sheets as Rds files in a newly created location.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_extract_dataset()
#'
#' @keywords internal
#'
#' @noRd

shs_extract_dataset <- function(source_dataset_path, extracted_dataset_path) {

  files <- list.files(source_dataset_path)

  years <- list()

  for (file in files) {
    years <- c(years, sub(".*SHS *(.*?) *_CH.*", "\\1", file))
  }

  year <- unique(years)

  # Get chapters and create subdirectories
  for (file in files) {
    chapter <- sub(paste0(".*", year, "_ *(.*?) *_.*"), "\\1", file)
    dir.create(file.path(extracted_dataset_path, chapter))
  }

  # Loop through files to list sheets and save each sheet as individual .Rda
  for (file in files) {
    workbook_path <- file.path(source_dataset_path, file)
    workbook <- XLConnect::loadWorkbook(workbook_path)
    sheets <- readxl::excel_sheets(workbook_path)

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

      # Reformat chapter number to match 'Data' directory structure
      chapter <- paste("CH", chapter_number, sep = "")

      # Read worksheet to dataframe
      df <- XLConnect::readWorksheet(workbook, sheet = sheet, header = TRUE)

      # Save dataframe as .Rds file
      saveRDS(df, file = file.path(extracted_dataset_path, chapter, paste0(dataframe_id, ".Rds")))
    }
  }



}
