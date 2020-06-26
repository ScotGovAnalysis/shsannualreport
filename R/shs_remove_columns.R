#' Remove columns from extracted datasets
#'
#' \code{shs_remove_columns} removes columns from extracted datasets.
#'
#' @param app_dataset_directory \code{string}.
#' The path to the directory the app will be created in.
#' @param columns_to_remove \code{character vector}.
#' The names of all columns to remove.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_remove_columns(columns_to_remove)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_remove_columns <- function(app_dataset_directory, columns_to_remove) {

  files <- list.files(app_dataset_directory)

  for (file in files) {

    file_path <- file.path(app_dataset_directory, file)

    df <- readRDS(file_path)
    column_names <- colnames(df)

    df <- df[, !(column_names %in% columns_to_remove)]

    saveRDS(df, file = file_path)
  }
}
