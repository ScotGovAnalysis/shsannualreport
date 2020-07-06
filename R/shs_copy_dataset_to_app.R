#' Copy existing app data to app directory
#'
#' \code{shs_copy_dataset_to_app} copies pre-processed app data and metadata files
#' to newly created app directories.
#'
#' @param data_directory \code{string}.
#' The path to the existing data directory.
#' @param destination_data_directory \code{string}.
#' The path to the data files will be stored in the app.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_copy_dataset_to_app(data_directory, destination_data_directory)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_copy_dataset_to_app <- function(data_directory, destination_data_directory) {

  dataset_directory <- file.path(data_directory, "dataset")
  metadata_directory <- file.path(data_directory, "metadata")

  destination_dataset_directory <- file.path(destination_data_directory, "dataset")
  destination_metadata_directory <- file.path(destination_data_directory, "metadata")

  dataset_files <- list.files(dataset_directory)

  for (dataset_file in dataset_files) {

    file.copy(file.path(dataset_directory, dataset_file), destination_dataset_directory)
  }

  metadata_files <- list.files(metadata_directory)

  for (metadata_file in metadata_files) {

    file.copy(file.path(metadata_directory, metadata_file), destination_metadata_directory)
  }
}
