# # This script allows a Shiny app to be made based on the SHS Annual Report, using Excel spreadsheets and the package shsannualreport.
# #
# # Before running the script:
# #                             * add chapter data to process to folder "source\dataset" in project
# #                             * add metadata spreadsheets to process ("chapter_titles.xlsx", "design_factors.xlsx", "question_titles.xlsx") to folder "source\metadata" in project
# #                             * add existing "column_names.xlsx" and "variable_names.xlsx" spreadsheets to variable_names_old (optional)
# #
# # When ready to run the script:
# #                             * make sure this script is commented out (CTRL + A, then CTRL + SHIFT + C)
# #                             * from the top menu select 'Build' > 'Clean and rebuild'
# #                             * run each line (stop and check spreadsheets at line 38 if necessary)
# #
# # When processing is finished, the app can be run by opening "app\app.R" and clicking 'Run app'
#
# library(shsannualreport)
#
# # Write data from Excel sheets in "source\dataset" and "source\metadata" to "app\data\dataset" and "app\data\metadata"
# shs_extract_data()
#
# # Remove specified columns from dataset (add any other unwanted columns to "columns_to_remove")
# columns_to_remove <- c("sort", "_LABEL_")
# shs_remove_columns(columns_to_remove)
#
# # # Write all column and variable names from data in "app\data\dataset" to Excel sheets in "variable_names_new"
# # shs_get_column_names()
# # shs_get_variable_names()
# #
# # # Update new column/variable names with values in folder "variable_names_old" (optional, only if populated files added to "variable_names_old")
# # # Note: This function works for either column or variable names, depending on the file names provided, so is used twice
# # new_workbook_path <- "variable_names_new\\column_names.xlsx"            # Column names sheet created by shs_get_column_names()
# # old_workbook_path <- file.path("variable_names_old\\column_names.xlsx")  # Set the old_workbook_path to spreadsheet in "variable_names_old"
# # shs_update_names_workbook(new_workbook_path, old_workbook_path)         # Update the new workbook with any names present in the old workbook
# #
# # # The process for variable names works exactly the same as for column names
# # new_workbook_path <- file.path("variable_names_new\\variable_names.xlsx")
# # old_workbook_path <- file.path("variable_names_old\\variable_names.xlsx")
# # shs_update_names_workbook(new_workbook_path, old_workbook_path)
#
# # STOP
# # At this point you can open the new column/variable names sheet and modify any display names you want
#
# # Extract the data from the column/variable names sheets to the extracted metadata folder
# shs_extract_column_and_variable_names()
#
# # Update the extracted dataset with the display column/variable names
# shs_process_column_names()
# shs_process_variable_names()
#
# # Process data (order rows, add statistical significance etc.), remove old file and re-save
# shs_process_data()
#
# # Create file "app\\source\\variables.R" based on processed data
# shs_shiny_variables()
#
