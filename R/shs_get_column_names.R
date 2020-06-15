#' Get column names from an extracted dataset
#'
#' \code{shs_get_column_names} gets column names from extracted SHS survey data in a specified location,
#' and saves them to an xlsx file, which can then be updated with names to display in the annual report Shiny app.
#'
#' @param app_dataset_directory \code{string}. The path of the directory the dataset has been extracted to.
#' @param column_names_save_file_path \code{string}. The path to save the column names spreadsheet to.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_get_column_names()
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_get_column_names <- function(app_dataset_directory, column_names_save_file_path) {

  files <- list.files(app_dataset_directory)
  all_column_names <- c()

  for (file in files) {

    file_path <- file.path(app_dataset_directory, file)
    column_names <- colnames(readRDS(file_path))
    all_column_names <- c(all_column_names, column_names)
  }

  all_column_names <- unique(all_column_names)
  all_column_names <- data.frame(all_column_names)
  colnames(all_column_names)[1] <- "source_name"
  all_column_names$display_name <- ""

  writexl::write_xlsx(list(column_names = all_column_names), path = column_names_save_file_path)
}
