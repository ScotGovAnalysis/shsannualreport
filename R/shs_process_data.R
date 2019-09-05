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

  # Data processing for type 1 datasets
  # E.g. council	[variable]	_2013	_2014	_2015	_2016	_2017	_All
  for (table in dplyr::filter(question_titles, Type == 1)$ID) {
    question_title <- gsub("/", " ", dplyr::filter(question_titles, ID == table)$Title)
    save_file_path <- file.path(extracted_dataset_path, paste0(question_title, ".Rds"))
    data_file_path <- file.path(extracted_dataset_path, data_files[grep(table, data_files)])
    print(data_file_path)
    shsannualreport:::shs_process_table_type_1(data_file_path, design_factors_path, save_file_path)
  }

  # Data processing for type 2 datasets
  # E.g council	[variable]	_2013	_All (with separate file for each year's data)
  for (table in dplyr::filter(question_titles, Type == 2)$ID) {
    question_title <- gsub("/", " ", dplyr::filter(question_titles, ID == table)$Title)
    save_file_path <- file.path(extracted_dataset_path, paste0(question_title, ".Rds"))
    files <- data_files[grepl(toupper(table), toupper(data_files))]
    final_df <- readRDS(file.path(extracted_dataset_path, files[1]))
    names(final_df)[3] <- "Percent"
    final_df$"Year" <- NA
    final_df <- final_df[0,]
    for (file in files) {
      data_file_path <- file.path(extracted_dataset_path, file)
      print(data_file_path)
      df <- shsannualreport:::shs_process_table_type_2(data_file_path, design_factors_path)
      final_df <- rbind(final_df, df)
      file.remove(data_file_path)
    }
    saveRDS(final_df, save_file_path)
  }

  # Data processing for type 3 datasets
  # E.g council	[variable]	_16to34	_35to64	_65	_All
  for (table in dplyr::filter(question_titles, Type == 3)$ID) {
    question_title <- gsub("/", " ", dplyr::filter(question_titles, ID == table)$Title)
    save_file_path <- file.path(extracted_dataset_path, paste0(question_title, ".Rds"))
    files <- data_files[grepl(toupper(table), toupper(data_files))]
    gather_key <- dplyr::filter(question_titles, ID == table)$Key
    column_3_name <- names(readRDS(file.path(extracted_dataset_path,files[1])))[2]
    colnames <- c("Year", "Council", column_3_name, gather_key, "Percent", "LowerConfidenceLimit", "UpperConfidenceLimit")
    final_df <- data.frame()
    for (colname in colnames) {
      final_df[[colname]]<-as.numeric()
    }

    for (file in files) {
      data_file_path <- file.path(extracted_dataset_path, file)
      print(data_file_path)
      df <- shsannualreport:::shs_process_table_type_3(data_file_path, design_factors_path, gather_key)
      final_df <- rbind(final_df, df)
      file.remove(data_file_path)
    }
    saveRDS(final_df, save_file_path)
  }

}
