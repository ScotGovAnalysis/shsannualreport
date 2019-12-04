#' Extract SHS survey data from Excel
#'
#' \code{shs_extract_data} extracts raw SHS survey data and metadata from Excel workbooks in a specified location,
#' and saves all sheets as Rds files in specified directories.
#' Uses internal functions \code{shs_extract_dataset} (to extract the source dataset) and \code{shs_extract_metadata} (to extract the source metadata).
#'
#' @param source_dataset_path \code{string}. The path of the source dataset. Should contain Excel workbooks of raw survey data.
#' @param source_metadata_path \code{string}. The path of the source metadata. Should contain Excel workbooks of chapter and question titles, and design factors.
#' @param extracted_data_path \code{string}. The path to extract the dataset and metadata to. Two directories will be created here; 'dataset' and 'metadata', and both will be populated with .Rds files.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_extract_data(source_dataset_path, source_metadata_path, extracted_data_path)
#' }
#'
#' @export

shs_extract_data <- function(source_dataset_path, source_metadata_path, extracted_data_path) {

  extracted_dataset_path <- file.path(extracted_data_path, "dataset")
  extracted_metadata_path <- file.path(extracted_data_path, "metadata")

  dir.create(extracted_dataset_path)
  dir.create(extracted_metadata_path)

  shsannualreport:::shs_extract_dataset(source_dataset_path,
                                       extracted_dataset_path)

  shsannualreport:::shs_extract_metadata(source_metadata_path,
                                        extracted_metadata_path)
}
