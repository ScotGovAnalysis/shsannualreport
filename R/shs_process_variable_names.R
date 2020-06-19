#' Rename variable names in extracted datasets
#'
#' \code{shs_process_variable_names} renames variables in extracted datasets
#' according to data specified in \code{variable_names.Rds} in extracted metadata.
#' This metadata is extracted from an Excel sheet \code{variable_names.xlsx}.
#' For more information see \code{shs_extract_data} and the internal function
#' \code{shs_extract_metadata}.
#'
#' @param app_dataset_directory \code{string}.
#' The path of the app directory containing the dataset.
#' @param app_metadata_directory \code{string}.
#' The path of the app directory containing metadata.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_process_variable_names(app_dataset_directory, app_metadata_directory)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_process_variable_names <- function(app_dataset_directory, app_metadata_directory) {

  variable_reference <- readRDS(file.path(app_metadata_directory, "variable_names.Rds"))

  variable_reference$display_name[is.na(variable_reference$display_name)] <-
    variable_reference$source_name[is.na(variable_reference$display_name)]

  files <- list.files(app_dataset_directory)

  for (file in files) {

    file_path <- file.path(app_dataset_directory, file)

    df <- readRDS(file_path)
    variable_names <- unique(df[, 2][[1]])

    for (variable_name in variable_names) {

      new_variable_name <- variable_reference[variable_reference$source_name == variable_name, 2]$display_name

      tryCatch({

      df[df[2] == variable_name, ][2] <- new_variable_name

      }, error = function(cond) {

        message(paste0("Error processing file: ", file, " Error msg: ", cond))
      })
    }
    saveRDS(df, file = file_path)
  }
}
