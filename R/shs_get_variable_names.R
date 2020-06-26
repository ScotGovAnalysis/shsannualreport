#' Get variable names from an extracted dataset
#'
#' \code{shs_get_variable_names} gets variable names from extracted SHS survey data in a specified location,
#' and saves them to an xlsx file, which can then be updated with names to display the annual report Shiny app.
#'
#' @param app_dataset_directory \code{string}. The path of the directory the dataset has been extracted to.
#' @param variable_names_save_file_path \code{string}. The path to save the variable names spreadsheet to.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_get_variable_names()
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_get_variable_names <- function(app_dataset_directory, variable_names_save_file_path) {

  files <- list.files(app_dataset_directory)
  all_variable_names <- c()

  for (file in files) {

    file_path <- file.path(app_dataset_directory, file)
    df <- readRDS(file_path)

    variable_names <- unique(df[2])[[1]]
    all_variable_names <- c(all_variable_names, variable_names)
  }

  all_variable_names <- unique(all_variable_names)
  all_variable_names <- data.frame(all_variable_names)
  colnames(all_variable_names)[1] <- "source_name"
  all_variable_names$display_name <- ""

  writexl::write_xlsx(list(variable_names = all_variable_names), path = variable_names_save_file_path)
}
