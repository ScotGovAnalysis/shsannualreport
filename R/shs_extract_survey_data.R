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

  # Set source data directory
  # TODO: make this a global variable, so can be used in other functions (poss. .REnviron)
  source_data_directory <- "source_data"

  #List all files in source data directory
  source_files <- list.files(source_data_directory)

  # Get all years named in source data file names
  years <- list()
  for (source_file in source_files) {
    years <- c(years, sub(".*SHS *(.*?) *_CH.*", "\\1", source_file))
  }

  #TODO: Make this into actual error
  # if (length(unique(years)) > 1) {
  #   print("Data is for more than one year")
  # }

  year <- unique(years)

  directory <- paste0(year, "_data_files")

  # Make directory based year of data
  # TODO: Should have check if directory exists
  dir.create(directory)

  #Get chapters and create subdirectories
  for(source_file in source_files) {
    chapter <- sub(paste0(".*", year, "_ *(.*?) *_.*"), "\\1", source_file)
    dir.create(paste0(directory, "\\", chapter))
  }

  for(source_file in source_files) {
    workbook <- XLConnect::loadWorkbook(file.path(source_data_directory, file))
    sheets <- readxl::excel_sheets(file.path(source_data_directory, file))
  }

}
