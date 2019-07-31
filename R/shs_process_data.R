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
#' shs_process_data(extracted_data_path)
#'
#' @export


shs_process_data <- function(extracted_data_path) {

  # Set source directories
  extracted_dataset_path <- file.path(extracted_data_path, "dataset")
  extracted_metadata_path <- file.path(extracted_data_path, "metadata")

  # Tasks to do:

  # Process column_names
  shsannualreport:::shs_process_column_names(extracted_dataset_path, extracted_metadata_path)

  # Combine datasets split over multiple years into single datasets
  shsannualreport:::shs_process_combine_multiple_years(extracted_dataset_path)

  # Design factors

  # Statistical significance

  # Add more...

}
