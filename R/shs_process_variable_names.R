#' Rename variable names in extracted datasets
#'
#' \code{shs_process_variable_names} renames variables in extracted datasets according to data specified in \code{variable_names.Rds} in extracted metadata.
#' This metadata is extracted from an Excel sheet \code{variable_names.xlsx}. For more information see \code{shs_extract_data} and the internal function
#' \code{shs_extract_metadata}.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_process_variable_names()
#' }
#'
#' @export

shs_process_variable_names <- function() {

  extracted_dataset_path <- "app\\data\\dataset"
  extracted_metadata_path <- "app\\data\\metadata"

  variable_reference <- readRDS(file.path(extracted_metadata_path, "variable_names.Rds"))

  variable_reference$display_name[is.na(variable_reference$display_name)] <-
    variable_reference$source_name[is.na(variable_reference$display_name)]

  files <- list.files(extracted_dataset_path)

  for (file in files){

    file_path <- file.path(extracted_dataset_path, file)

    df <- readRDS(file_path)
    variable_names <- unique(df[,2][[1]])

    for (variable_name in variable_names){

      new_variable_name <- variable_reference[variable_reference$source_name == variable_name, 2]$display_name

      tryCatch({

      df[df[2] == variable_name,][2] <- new_variable_name

      }, error = function(cond) {

        message(paste0("Error processing file: ", file, " Error msg: ", cond))
      })
    }
    saveRDS(df, file = file_path)
  }
}
