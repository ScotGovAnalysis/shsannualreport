#' Rename column names in extracted datasets
#'
#' \code{shs_process_column_names} renames columns in extracted datasets according to data specified in \code{column_names.Rds} in extracted metadata.
#' This metadata is extracted from an Excel sheet \code{column_names.xlsx}. For more information see \code{shs_extract_data} and the internal function
#' \code{shs_extract_metadata}.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_process_column_names()
#' }
#'
#' @export

shs_process_column_names <- function() {

  extracted_dataset_path <- "app/data/dataset"
  extracted_metadata_path <- "app/data/metadata"

  column_reference <- readRDS(file.path(extracted_metadata_path, "column_names.Rds"))

  column_reference$display_name[is.na(column_reference$display_name)] <-
    column_reference$source_name[is.na(column_reference$display_name)]

  files <- list.files(extracted_dataset_path)

  for (file in files) {

    file_path <- file.path(extracted_dataset_path, file)
    df <- readRDS(file_path)
    column_names <- colnames(df)

    for (column_name in column_names) {

      new_column_name <- column_reference[column_reference$source_name == column_name, 2]

      tryCatch({
        colnames(df)[colnames(df) == column_name] <- new_column_name},
        warnings = function(w) {
          message(paste0("File Path:", file_path, " Column Name: ", column_name, "Warning: ", w))})
    }

    saveRDS(df, file = file_path)
  }
}

