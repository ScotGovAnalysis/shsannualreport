# library(shsannualreport)
#
# # Add source files to these locations
# # source_dataset_path is where the Excel sheets of raw data should go
# # source_metadata_path is where the chapter_titles.xlsx, design_factors.xlsx, and question_titles.xlsx should go
# source_dataset_path <- "C:\\Users\\dsap01\\Documents\\shsannualreportdata\\source\\dataset"
# source_metadata_path <- "C:\\Users\\dsap01\\Documents\\shsannualreportdata\\source\\metadata"
#
# # Make sure this folder does not contain folders called column_names.xlsx or variable_names.xlsx
# # This path is where spreadsheets of column and variable names will be extracted to from the dataset
# # These spreadsheets will be created, so there shouldn't be existing spreadsheets in the folder with the same name
# new_column_and_variable_path <- "C:\\Users\\dsap01\\Documents\\shsannualreportdata\\column_and_variable_names_new"
#
# # Add populated spreadsheets here
# # This should contain column_names.xlsx and variable_names.xlsx, for a previous year's data, where the display_name field is populated.
# old_column_and_variable_path <- "C:\\Users\\dsap01\\Documents\\shsannualreportdata\\column_and_variable_names_old"
#
# # Make sure this folder is empty
# extracted_data_path <- "C:\\Users\\dsap01\\Documents\\shsannualreportdata\\extracted_data"
#
# # This writes the data from the Excel sheets in the source_dataset_path to the extracted_data_path, as .Rds files
# shs_extract_data(source_dataset_path, source_metadata_path, extracted_data_path)
#
# # shs_extract_data creates dataset and metadata directories which can be accessed as so:
# extracted_dataset_path <- paste0(extracted_data_path, "\\dataset")
# extracted_metadata_path <- paste0(extracted_data_path, "\\metadata")
#
# # Removes unnecessary columns, to remove others add the name of the column to the list
# shs_remove_columns(extracted_data_path, c("sort", "_LABEL_"))
#
# # Writes all column and variable names from the extracted_data_path to Excel sheets in the new_column_and_variable_path
# shs_get_column_names(extracted_data_path, new_column_and_variable_path)
# shs_get_variable_names(extracted_data_path, new_column_and_variable_path)
#
# # This function works for either column or variable names, depending on the file names provided.
# # To process column names, set the new_column_and_variable path to the file that was created by shs_get_column_names
# new_workbook_path <- file.path(new_column_and_variable_path, "column_names.xlsx")
# # Set the old_column_and_variable_path to a previous year's column_names spreadsheet
# old_workbook_path <- file.path(old_column_and_variable_path, "column_names.xls")
# # shs_update_names_workbook update the new workbook with any names present in the old workbook
# shs_update_names_workbook(new_workbook_path, old_workbook_path)
#
# # The process for variable names works exactly the same as for column names
# new_workbook_path <- file.path(new_column_and_variable_path, "variable_names.xlsx")
# old_workbook_path <- file.path(old_column_and_variable_path, "variable_names.xls")
# shs_update_names_workbook(new_workbook_path, old_workbook_path)
#
# # At this point you can open the new column/variable names sheet and modify any display names you want
#
# # This extracts the data from the column/variable names sheets to the extracted metadata folder
# shs_extract_column_and_variable_names(new_column_and_variable_path, extracted_metadata_path)
#
# # These update the extracted dataset with the display column/variable names
# shs_process_column_names(extracted_data_path)
# shs_process_variable_names(extracted_data_path)
#
# # These process the data (add statistical significance etc.)
# shs_process_data(extracted_data_path)
#
# # The code below updates NULL/NA values with "-" or "*"
# files <- list.files(extracted_dataset_path)
#
# # This is a list of years with incomplete data. Any missing values for years in this list will be populated with "*"
# incomplete_years <- c("2011")
#
# for (file in files) {
#
#   data_file_path <- file.path(extracted_dataset_path, file)
#
#   tryCatch({
#   shs_replace_na(data_file_path, incomplete_years)
#   }, error = function(cond) {
#     message(paste0("Error in test script replacing nulls and NAs for file: ", file, " Error msg: ", cond))
#   })
# }
#
# shs_shiny_variables("variables.R", extracted_data_path)
