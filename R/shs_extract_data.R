#' Extract SHS data from Excel workbooks
#'
#' \code{shs_extract_data} extracts raw data from Excel workbooks in a specified location,
#' and saves all sheets as Rds files in a newly created location.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_extract_data()
#'
#' @export


# Set up input and output directories
shs_extract_data <- function() {

source_survey_data_path <- file.path("source_data", "survey_data")

source_titles_path <- file.path("source_data", "titles")

# TODO: set as datetime in saveable format
extracted_data_path <- paste("extracted_data", Sys.Date())

extracted_survey_data_path <- file.path(extracted_data_path, "survey_data")

extracted_titles_data_path <- file.path(extracted_data_path, "titles")

dir.create(extracted_data_path)

dir.create(extracted_survey_data_path)

dir.create(extracted_titles_data_path)

shs_extract_survey_data(source_survey_data_path, extracted_survey_data_path)

shs_extract_titles(source_titles_path, extracted_titles_data_path)

}
