# library(shsannualreport)
#
# source_data_directory <- "C:/Users/dsap01/Documents/shsannualreportdata"
# top_level_directory <- "C:/Users/dsap01/Documents"
# columns_to_remove <- c("sort", "_LABEL_", "var", "LABEL")
# old_column_names_path <- "C:/Users/dsap01/Documents/shsannualreportdata/variable_names_old/column_names.xls"
# old_variable_names_path <- "C:/Users/dsap01/Documents/shsannualreportdata/variable_names_old/variable_names.xls"
# reports_start_year <- 2013
# reports_end_year <- 2018
#
# source_dataset_directory <- file.path(source_data_directory, "dataset")
# source_metadata_directory <- file.path(source_data_directory, "metadata")
#
# time <- gsub(" ", "-", Sys.time())
# time <- gsub(":", "-", time)
#
# top_level_directory <- file.path(top_level_directory, time)
#
# app_directory <- file.path(top_level_directory, "app")
# app_data_directory <- file.path(app_directory, "data")
# app_dataset_directory <- file.path(app_data_directory, "dataset")
# app_metadata_directory <- file.path(app_data_directory, "metadata")
# app_source_directory <- file.path(app_data_directory, "source")
# app_reports_directory <- file.path(app_data_directory, "reports")
#
# unlink(app_dataset_directory, recursive = TRUE)
# unlink(app_metadata_directory, recursive = TRUE)
#
# dir.create(top_level_directory)
# dir.create(app_directory)
# dir.create(app_data_directory)
# dir.create(app_dataset_directory)
# dir.create(app_metadata_directory)
# dir.create(app_source_directory)
# dir.create(app_reports_directory)
#
# column_names_save_file_path <- file.path(source_metadata_directory, "column_names.xlsx")
# variable_names_save_file_path <- file.path(source_metadata_directory, "variable_names.xlsx")
#
# tryCatch({
#   message(paste0("Extracting metadata from ", source_metadata_directory, " to ", app_metadata_directory))
#   shsannualreport:::shs_extract_metadata(source_metadata_directory = source_metadata_directory, app_metadata_directory = app_metadata_directory)
#   message("Successfully extracted metadata")
# }, error = function(e) {
#   message("Failed to extract metadata")
#   message(e)
# })
#
# tryCatch({
#   message(paste0("Extracting dataset from ", source_dataset_directory, " to ", app_dataset_directory))
# shsannualreport:::shs_extract_dataset(source_dataset_directory = source_dataset_directory, app_dataset_directory = app_dataset_directory)
#   message("Successfully extracted dataset")
# }, error = function(e) {
#   message("Failed to extract data")
#   message(e)
# })
#
# tryCatch({
#   message(paste0("Removing columns ", paste(columns_to_remove, collapse = ", "), " from data in ", app_dataset_directory))
#   shsannualreport:::shs_remove_columns(app_dataset_directory = app_dataset_directory, columns_to_remove = columns_to_remove)
#   message("Successfully removed columns")
# }, error = function(e) {
#   message("Failed to remove columns")
#   message(e)
# })
#
# tryCatch({
#   message(paste0("Writing column names present in ", app_dataset_directory, " to ", column_names_save_file_path))
#   shsannualreport:::shs_get_column_names(app_dataset_directory = app_dataset_directory, column_names_save_file_path = column_names_save_file_path)
#   message("Successfully wrote column names")
# }, error = function(e) {
#   message("Failed to write column names")
#   message(e)
# })
#
# tryCatch({
#   message(paste0("Writing variable names present in ", app_dataset_directory, " to ", variable_names_save_file_path))
#   shsannualreport:::shs_get_variable_names(app_dataset_directory = app_dataset_directory, variable_names_save_file_path = variable_names_save_file_path)
#   message("Successfully wrote variable names")
# }, error = function(e) {
#   message("Failed to write variable names")
#   message(e)
# })
#
# if (file.exists(old_column_names_path)) {
# tryCatch({
#   message(paste0("Writing column names present in ", old_column_names_path, " to ", column_names_save_file_path))
#   shsannualreport:::shs_update_names_workbook(new_workbook_path = column_names_save_file_path, old_workbook_path = old_column_names_path)
#   message("Successfully wrote column names")
# }, error = function(e) {
#   message("Failed to write column names")
#   message(e)
# })
#
# } else {
#
#   message(paste0("The value given for 'old_column_names_path' does not exist, so ", column_names_save_file_path, " has not been updated."))
#   message(paste0("The value given was ", old_column_names_path))
#   }
#
# if (file.exists(old_variable_names_path)) {
# tryCatch({
#   message(paste0("Writing variable names present in ", old_variable_names_path, " to ", variable_names_save_file_path))
#   shsannualreport:::shs_update_names_workbook(new_workbook_path = variable_names_save_file_path, old_workbook_path = old_variable_names_path)
#   message("Successfully wrote variable names")
# }, error = function(e) {
#   message("Failed to write variable names")
#   message(e)
# })
#
# } else {
#
#   message(paste0("The value given for 'old_variable_names_path' does not exist, so ", variable_names_save_file_path, " has not been updated."))
#   message(paste0("The value given was", old_variable_names_path))
# }
#
# tryCatch({
#   message(paste0("Extracting  ", column_names_save_file_path, " and ", variable_names_save_file_path, " to ", app_metadata_directory))
#   shsannualreport:::shs_extract_column_and_variable_names(app_metadata_directory = app_metadata_directory, column_names_save_file_path = column_names_save_file_path, variable_names_save_file_path = variable_names_save_file_path)
# message("Successfully extracted column and variable names")
# }, error = function(e) {
#   message("Failed to extract column and variable names")
#   message(e)
# })
#
# tryCatch({
#   message(paste0("Processing column names in ", app_dataset_directory))
#   shsannualreport:::shs_process_column_names(app_dataset_directory = app_dataset_directory, app_metadata_directory = app_metadata_directory)
# message("Successfully processed column names")
# }, error = function(e) {
#   message("Failed to process column names")
#   message(e)
# })
#
# tryCatch({
#   message(paste0("Processing variable names in ", app_dataset_directory))
#   shsannualreport:::shs_process_variable_names(app_dataset_directory = app_dataset_directory, app_metadata_directory = app_metadata_directory)
#   message("Successfully processed variable names")
# }, error = function(e) {
#   message("Failed to process variable names")
#   message(e)
# })
#
# tryCatch({
#   message(paste0("Processing data in ", app_dataset_directory))
#   shsannualreport:::shs_process_data(app_dataset_directory = app_dataset_directory, app_metadata_directory = app_metadata_directory)
#   message("Successfully processed data")
# }, error = function(e) {
#   message("Failed to process data")
#   message(e)
# })
#
# tryCatch({
#   message(paste0("Creating variables source file in ", app_source_directory))
#   shsannualreport:::shs_create_shiny_variables(reports_start_year = reports_start_year,
#                                         reports_end_year = reports_end_year,
#                                         app_source_directory = app_source_directory,
#                                         app_dataset_directory = app_dataset_directory,
#                                         app_metadata_directory = app_metadata_directory)
#   message("Successfully created variables file")
# }, error = function(e) {
#   message("Failed to create variables file")
#   message(e)
# })
#
# tryCatch({
#   message(paste0("Creating reports files in ", app_reports_directory))
#   shsannualreport:::shs_create_reports(app_metadata_directory = app_metadata_directory, app_reports_directory = app_reports_directory)
#   message("Successfully created reports files")
# }, error = function(e) {
#   message("Failed to create reports files")
#   message(e)
# })
#
