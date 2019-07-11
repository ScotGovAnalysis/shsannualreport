#' Extract data from Excel workbooks
#'
#' \code{shs_extract_survey_data} extracts data from Excel workbooks in a specified location,
#' and saves all sheets as RData files in a newly created location.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_extract_survey_data()
#'
#' @export

shs_extract_survey_data <- function() {

  # Set source data directory (use shs_set_source_data_directory to set up)
  source_data_directory <- Sys.getenv("source_data_directory")

  #List all source data files
  source_files <- list.files(source_data_directory)

  # Get all years named in source data file names
  years <- list()
  for (source_file in source_files) {
    years <- c(years, sub(".*SHS *(.*?) *_CH.*", "\\1", source_file))
  }

  year <- unique(years)

  if (length(year) > 1) {
    stop("The provided source data directory contains more than one year's data.
         Please check the data and try again.")
  }

  directory <- paste0(year, "_data_files")

  # Make directory based year of data
  if (dir.exists(directory) == FALSE) {
  dir.create(directory)
  } else {
    stop("Data for this year has already been created. Please delete or move existing data before proceeding")
  }

  #Get chapters and create subdirectories
  for(source_file in source_files) {
    chapter <- sub(paste0(".*", year, "_ *(.*?) *_.*"), "\\1", source_file)
    dir.create(paste0(directory, "\\", chapter))
  }

  # Loop through files to list sheets and save each sheet as individual .Rda
  for(source_file in source_files) {
    workbook_path <- file.path(source_data_directory, source_file)
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
        stop(paste0("Unknown type of data in sheet ", sheet, ". Only 'FIG' or 'TAB' sheets permitted."))
      }

      # Reformat chapter number to match 'Data' directory structure
      chapter <- paste("CH", chapter_number, sep = "")

      # Read worksheet to dataframe
      df <- XLConnect::readWorksheet(workbook, sheet = sheet, header = TRUE)

      # Save dataframe as .Rds file
      saveRDS(df, file = paste0(directory, "\\", chapter, "\\", dataframe_id, ".Rds"))
    }
  }

}
