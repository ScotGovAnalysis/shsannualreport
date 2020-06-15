#' First part of SHS Data Explorer app processing
#'
#' \code{shs_app_processing_1} extracts data and metadata, removes unnecessary columns, and creates Excel files of variable names for review.
#'
#' @param top_level_directory \code{string}. The path of the directory the app will be created in.
#' @param columns_to_remove \code{string}. Unnecessary columns to remove from the dataset.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_app_processing_1(top_level_directory, columns_to_remove, old_column_names_path, old_variable_names_path)
#' }
#'
#' @export

shs_app_processing_1 <- function(top_level_directory,
                                 columns_to_remove) {

  source_dataset_directory <- file.path(source_data_directory, "dataset")
  source_metadata_directory <- file.path(source_data_directory, "metadata")

  app_directory <- file.path(top_level_directory, "app")
  app_data_directory <- file.path(app_directory, "data")
  app_dataset_directory <- file.path(app_data_directory, "dataset")
  app_metadata_directory <- file.path(app_data_directory, "metadata")

  column_names_save_file_path <- file.path(source_metadata_directory, "column_names.xlsx")
  variable_names_save_file_path <- file.path(source_metadata_directory, "variable_names.xlsx")

  old_column_names_path <- file.path(source_metadata_directory, "old_column_names.xlsx")
  old_variable_names_path <- file.path(source_metadata_directory, "old_variable_names.xlsx")

  unlink(app_dataset_directory, recursive = TRUE)
  unlink(app_metadata_directory, recursive = TRUE)

  dir.create(top_level_directory)
  dir.create(app_directory)
  dir.create(app_data_directory)
  dir.create(app_dataset_directory)
  dir.create(app_metadata_directory)

  tryCatch({
    message(paste0("Extracting metadata from ", source_metadata_directory, " to ", app_metadata_directory))
    shsannualreport:::shs_extract_metadata(source_metadata_directory = source_metadata_directory, app_metadata_directory = app_metadata_directory)
    message("Successfully extracted metadata")
  }, error = function(e) {
    message("Failed to extract metadata")
    message(e)
  })

  tryCatch({
    message(paste0("Extracting dataset from ", source_dataset_directory, " to ", app_dataset_directory))
    shsannualreport:::shs_extract_dataset(source_dataset_directory = source_dataset_directory, app_dataset_directory = app_dataset_directory)
    message("Successfully extracted dataset")
  }, error = function(e) {
    message("Failed to extract data")
    message(e)
  })

  tryCatch({
    message(paste0("Removing columns ", paste(columns_to_remove, collapse = ", "), " from data in ", app_dataset_directory))
    shsannualreport:::shs_remove_columns(app_dataset_directory = app_dataset_directory, columns_to_remove = columns_to_remove)
    message("Successfully removed columns")
  }, error = function(e) {
    message("Failed to remove columns")
    message(e)
  })

  tryCatch({
    message(paste0("Writing column names present in ", app_dataset_directory, " to ", column_names_save_file_path))
    shsannualreport:::shs_get_column_names(app_dataset_directory = app_dataset_directory, column_names_save_file_path = column_names_save_file_path)
    message("Successfully wrote column names")
  }, error = function(e) {
    message("Failed to write column names")
    message(e)
  })

  tryCatch({
    message(paste0("Writing variable names present in ", app_dataset_directory, " to ", variable_names_save_file_path))
    shsannualreport:::shs_get_variable_names(app_dataset_directory = app_dataset_directory, variable_names_save_file_path = variable_names_save_file_path)
    message("Successfully wrote variable names")
  }, error = function(e) {
    message("Failed to write variable names")
    message(e)
  })

  if (file.exists(old_column_names_path)) {
    tryCatch({
      message(paste0("Writing column names present in ", old_column_names_path, " to ", column_names_save_file_path))
      shsannualreport:::shs_update_names_workbook(new_workbook_path = column_names_save_file_path, old_workbook_path = old_column_names_path)
      message("Successfully wrote column names")
    }, error = function(e) {
      message("Failed to write column names")
      message(e)
    })

  } else {

    message(paste0("The value given for 'old_column_names_path' does not exist, so ", column_names_save_file_path, " has not been updated."))
    message(paste0("The value given was ", old_column_names_path))
  }

  if (file.exists(old_variable_names_path)) {
    tryCatch({
      message(paste0("Writing variable names present in ", old_variable_names_path, " to ", variable_names_save_file_path))
      shsannualreport:::shs_update_names_workbook(new_workbook_path = variable_names_save_file_path, old_workbook_path = old_variable_names_path)
      message("Successfully wrote variable names")
    }, error = function(e) {
      message("Failed to write variable names")
      message(e)
    })

  } else {

    message(paste0("The value given for 'old_variable_names_path' does not exist, so ", variable_names_save_file_path, " has not been updated."))
    message(paste0("The value given was", old_variable_names_path))
  }

  message("Processing completed successfully")
  message(paste0("Please review ", column_names_save_file_path, " and ", variable_names_save_file_path, " and run shs_app_processing_2"))
}
