#' Rename column names in extracted datasets
#'
#' \code{shs_process_column_names} renames columns in extracted datasets according to data specified \code{column_names.Rds} in extracted metadata.
#' This metadata is extracted from an Excel sheet \code{column_names.xlsx}. For more information see \code{shs_extract_data} and the internal function
#' \code{shs_extract_metadata}.
#'
#' @param extracted_dataset_path \code{string}. The path of the directory containing extracted survey data.
#' @param extracted_metadata_path \code{string}. The path of the directory containing extracted metadata.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_process_column_names(extracted_dataset_path, extracted_metadata_path)
#'
#' @keywords internal
#'
#' @noRd

shs_process_column_names <- function(extracted_dataset_path,
                               extracted_metadata_path) {

  # Load in column_names reference data
  column_reference <- readRDS(file.path(extracted_metadata_path,
                                     "column_names.Rds"))

  # Replace any NA values in display_name with source_name
  column_reference$display_name[is.na(column_reference$display_name)] <-
    column_reference$source_name[is.na(column_reference$display_name)]

  # List all files in dataset directory
  files <- list.files(extracted_dataset_path)

    #Loop through files
    for (file in files){

      file_path <- file.path(extracted_dataset_path, file)

      # Read in file and extract column names
      df <- readRDS(file_path)
      column_names <- colnames(df)

      #Loop through column names
      for (column_name in column_names) {

        # Get new column name from reference table
        new_column_name <- column_reference[column_reference$source_name
                                            == column_name, 2]

        # Update old column name
        tryCatch({
        colnames(df)[colnames(df) == column_name] <- new_column_name},
          error = function(e) {
            print(paste0(file_path, column_name))
          }
        )

        # Save updated file
        saveRDS(df, file = file_path)
      }
    }
  }

