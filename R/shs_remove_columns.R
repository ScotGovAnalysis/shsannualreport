#' Remove columns from extracted datasets
#'
#' \code{shs_remove_columns} removes columns from extracted datasets.
#'
#' @param extracted_dataset_path \code{string}. The path of the directory containing extracted survey data.
#' @param columns_to_remove \code{list}. The names of all columns to remove.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_remove_columns(extracted_dataset_path, columns_to_remove)
#'
#' @export

shs_remove_columns <- function(extracted_dataset_path, columns_to_remove) {

  # List all files in dataset directory
  files <- list.files(extracted_dataset_path)

  #Loop through files
  for (file in files){
    file_path <- file.path(extracted_dataset_path, file)

    # Read in file and extract column names
    df <- readRDS(file_path)
    column_names <- colnames(df)

    df <- df[, !(column_names %in% columns_to_remove)]

    saveRDS(df, file = file_path)
  }

}
