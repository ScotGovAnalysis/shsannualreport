#' Extract SHS survey data from Excel
#'
#' \code{shs_extract_data} extracts raw SHS survey data and metadata from Excel workbooks in a specified location,
#' and saves all sheets as Rds files in newly created directories.
#' Uses internal functions \code{shs_extract_dataset} (to extract the source dataset) and \code{shs_extract_metadata} (to extract the source metadata).
#'
#' @return \code{null}.
#'
#' @examples
#' shs_extract_data()
#'
#' @export


# Set up input and output directories
shs_extract_data <- function() {

  # Set source directories, could be passed as parameter later
  source_dataset_path <- file.path("source", "dataset")
  source_metadata_path <- file.path("source", "metadata")

  # Create names for output directories
  # (datetime added to top level folder in case of multiple runs)
  extracted_data_path <- paste("extracted",
                               format(Sys.time(), "%d-%b-%Y %H.%M.%S"))
  extracted_dataset_path <- file.path(extracted_data_path, "dataset")
  extracted_metadata_path <- file.path(extracted_data_path, "metadata")

  # Create output directories
  dir.create(extracted_data_path)
  dir.create(extracted_dataset_path)
  dir.create(extracted_metadata_path)

  # Extract dataset to output directory
  shsannualreport::shs_extract_dataset(source_dataset_path,
                                       extracted_dataset_path)

  # Extract metadata to output directory
  shsannualreport::shs_extract_metadata(source_metadata_path,
                                        extracted_metadata_path)

}
