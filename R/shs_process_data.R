#' Process SHS annual report data for publication
#'
#' \code{shs_process_data} processes raw SHS survey data which has been extracted from Excel sheets into individual .Rds files.
#' It relies on a number of internal functions: \code{shs_process_column_names}, TODO: update as more functions added.
#'
#' @param extracted_data_path \code{string}. The path of the directory data has previously been extracted to, by \code{shs_extract_data}.
#'
#' @return \code{null}.
#'
#' @examples
#'
#' \dontrun{
#' shs_process_data("extracted_data_path")
#' }
#'
#' @export

shs_process_data <- function(extracted_data_path) {

  # Set source directories
  extracted_dataset_path <- file.path(extracted_data_path, "dataset")
  extracted_metadata_path <- file.path(extracted_data_path, "metadata")

  # Process column_names
  shsannualreport:::shs_process_column_names(extracted_dataset_path, extracted_metadata_path)

  question_titles <- readRDS(file.path(extracted_metadata_path, "question_titles.Rds"))
  data_files <- list.files(extracted_dataset_path)
  design_factors_path <- file.path(extracted_metadata_path, "design_factors.Rds")

  # Combine datasets split over multiple years into single datasets
  # Probably won't need this anymore
  # shsannualreport:::shs_process_combine_multiple_years(extracted_dataset_path)

  # Data processing for type 1 datasets
  for (table in dplyr::filter(question_titles, Type == 1)$ID) {
    question_title <- gsub("/", " ", dplyr::filter(question_titles, ID == table)$Title)
    save_file_path <- file.path(extracted_dataset_path, paste0(question_title, ".Rds"))
    data_file_path <- file.path(extracted_dataset_path, data_files[grep(table, data_files)])
    shsannualreport:::shs_process_table_type_1(data_file_path, design_factors_path, save_file_path)
  }

  # Data processing for type 2 datasets
  for (table in dplyr::filter(question_titles, Type == 2)$ID) {
    question_title <- gsub("/", " ", dplyr::filter(question_titles, ID == table)$Title)
    save_file_path <- file.path(extracted_dataset_path, paste0(question_title, ".Rds"))
    files <- data_files[grepl(toupper(table), toupper(data_files))]
    final_df <- readRDS(file.path(extracted_dataset_path, files[1]))
    names(final_df)[3] <- "Percent"
    #Remove when renaming completed
    # names(df)[3] <- "temp_variable_name"
    final_df$"Year" <- NA
    final_df <- final_df[0,]
    for (file in files) {
      data_file_path <- file.path(extracted_dataset_path, file)
      df <- shsannualreport:::shs_process_table_type_2(data_file_path, design_factors_path)
      final_df <- rbind(final_df, df)
      file.remove(data_file_path)
    }
    saveRDS(final_df, save_file_path)
  }
}
