#' Rename variable names in extracted datasets
#'
#' \code{shs_process_variable_names} renames variables in extracted datasets according to data specified \code{variable_names.Rds} in extracted metadata.
#' This metadata is extracted from an Excel sheet \code{variable_names.xlsx}. For more information see \code{shs_extract_data} and the internal function
#' \code{shs_extract_metadata}.
#'
#' @param extracted_dataset_path \code{string}. The path of the directory containing extracted survey data.
#' @param extracted_metadata_path \code{string}. The path of the directory containing extracted metadata.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_process_variable_names(extracted_dataset_path, extracted_metadata_path)
#'
#' @export

shs_process_variable_names <- function(extracted_data_path) {

  # Set source directories
  extracted_dataset_path <- file.path(extracted_data_path, "dataset")
  extracted_metadata_path <- file.path(extracted_data_path, "metadata")

  # Load in variable_names reference data
  variable_reference <- readRDS(file.path(extracted_metadata_path,
                                        "variable_names.Rds"))

  # Replace any NA values in display_name with source_name
  variable_reference$display_name[is.na(variable_reference$display_name)] <-
    variable_reference$source_name[is.na(variable_reference$display_name)]

  # List all files in dataset directory
  files <- list.files(extracted_dataset_path)

  #Loop through files
  for (file in files){

    file_path <- file.path(extracted_dataset_path, file)

    # Read in file and extract variable names
    df <- readRDS(file_path)
    variable_names <- unique(df[,2][[1]])

    for (variable_name in variable_names){

      # Get new variable name from reference table
      new_variable_name <- variable_reference[variable_reference$source_name
                                          == variable_name, 2]$display_name

      df[df[2] == variable_name,][2] <- new_variable_name
    }
    saveRDS(df, file = file_path)
  }
}
