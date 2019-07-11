#' Extract SHS data from Excel workbooks
#'
#' \code{shs_extract_data} extracts raw survey data and metadata from Excel workbooks in a specified location,
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

  source_dataset_path <- file.path("source", "dataset")

  source_metadata_path <- file.path("source", "metadata")

  # TODO: set as datetime in saveable format

  extracted_data_path <- paste("extracted", Sys.Date())

  extracted_dataset_path <- file.path(extracted_data_path, "dataset")

  extracted_metadata_path <- file.path(extracted_data_path, "metadata")

  dir.create(extracted_data_path)

  dir.create(extracted_dataset_path)

  dir.create(extracted_metadata_path)

  shs_extract_dataset(source_dataset_path, extracted_dataset_path)

  shs_extract_metadata(source_metadata_path, extracted_metadata_path)

}
