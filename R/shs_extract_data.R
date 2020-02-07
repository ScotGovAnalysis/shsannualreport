#' Extract SHS survey data from Excel
#'
#' \code{shs_extract_data} extracts raw SHS survey data and metadata from Excel workbooks in a specified location,
#' and saves all sheets as Rds files in specified directories.
#' Uses internal functions \code{shs_extract_dataset} (to extract the source dataset) and \code{shs_extract_metadata} (to extract the source metadata).
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_extract_data()
#' }
#'
#' @export

shs_extract_data <- function() {

  source_dataset_path <- "source\\dataset"
  source_metadata_path <- "source\\metadata"

  extracted_dataset_path <- "app\\data\\dataset"
  extracted_metadata_path <- "app\\data\\metadata"

  unlink(extracted_dataset_path, recursive = TRUE)
  unlink(extracted_metadata_path, recursive = TRUE)

  dir.create(extracted_dataset_path)
  dir.create(extracted_metadata_path)

  shsannualreport:::shs_extract_dataset(source_dataset_path,
                                       extracted_dataset_path)

  shsannualreport:::shs_extract_metadata(source_metadata_path,
                                        extracted_metadata_path)
}
