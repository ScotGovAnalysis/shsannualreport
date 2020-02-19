#' Get column names from an extracted dataset
#'
#' \code{shs_get_column_names} gets column names from extracted SHS survey data in a specified location,
#' and saves them to an xlsx file, which can then be updated with names to display in the annual report Shiny app.
#'
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_get_column_names()
#' }
#'
#' @export

shs_get_column_names <- function() {

  extracted_dataset_path <- "app\\data\\dataset"
  files <- list.files(extracted_dataset_path)
  all_column_names <- c()

  for (file in files) {

    file_path <- file.path(extracted_dataset_path, file)
    column_names <- colnames(readRDS(file_path))
    all_column_names <- c(all_column_names, column_names)
  }

  all_column_names <- unique(all_column_names)
  all_column_names <- data.frame(all_column_names)
  colnames(all_column_names)[1] <- "source_name"
  all_column_names$display_name <- ""

  dir.create("variable_names_new")

  writexl::write_xlsx(list(column_names = all_column_names), path = "variable_names_new\\column_names.xlsx")
}
