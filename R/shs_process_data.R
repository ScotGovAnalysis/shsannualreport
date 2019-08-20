#' Process SHS annual report data for publication
#'
#' \code{shs_process_data} processes raw SHS survey data which has been extracted from Excel sheets into individual .Rds files.
#' It relies on a number of internal functions: \code{shs_process_column_names}, TODO: update as more functions added.
#'
#' @param extracted_data_path \code{string}. The path of the directory data has previously been extracted to, by \code{shs_extract_data}.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_process_data("extracted_data_path")
#'
#' @export

shs_process_data <- function(extracted_data_path) {

  # Set source directories
  extracted_dataset_path <- file.path(extracted_data_path, "dataset")
  extracted_metadata_path <- file.path(extracted_data_path, "metadata")

  # Process column_names
  shsannualreport::shs_process_column_names(extracted_dataset_path, extracted_metadata_path)

  # Combine datasets split over multiple years into single datasets
  # shsannualreport:::shs_process_combine_multiple_years(extracted_dataset_path)

  # Data processing for type 1 datasets
  question_titles <- readRDS(file.path(extracted_metadata_path, "question_titles.Rds"))
  data_files <- list.files(extracted_dataset_path)
  for (table in filter(question_titles, Type <= 1)$ID) {
    data_file_path <- file.path(extracted_dataset_path, data_files[grep(table, data_files)])
    design_factors_path <- file.path(extracted_metadata_path, "design_factors.Rds")
    shsannualreport::shs_process_table_type_1(data_file_path, design_factors_path)
  }
}
