#' Get column names from an extracted dataset
#'
#' \code{shs_get_column_names} gets column names from extracted SHS survey data in a specified location,
#' and saves them to an xlsx file, which can then be updated with names to display the annual report Shiny app.
#'
#' @param extracted_dataset_path \code{string}. The path of the directory containing extracted survey data.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_get_column_names(extracted_dataset_path)
#'
#' @export

shs_get_column_names <- function(extracted_dataset_path) {

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

writexl::write_xlsx(all_column_names, path = "column_names.xlsx")

}
