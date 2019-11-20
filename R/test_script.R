# library(shsannualreport)
#
# shs_extract_data()
#
# #TODO set this value
# extracted_data_path <- "extracted 20-Nov-2019 15.55.01"
#
# extracted_dataset_path <- paste0(extracted_data_path, "\\dataset")
# extracted_metadata_path <- paste0(extracted_data_path, "\\metadata")
#
# shs_remove_columns(extracted_dataset_path, c("sort", "_LABEL_"))
#
# shs_get_column_names(extracted_data_path)
# shs_get_variable_names(extracted_data_path)
#
# #TODO set these values
# new_workbook_path <- "C:\\Users\\dsap01\\Documents\\shsannualreport\\source\\column_and_variable_names\\column_names.xlsx"
# old_workbook_path <- "C:\\Users\\dsap01\\Documents\\shsannualreport\\test_column_and_variable_names_to_update\\column_names_to_update.xls"
# shs_update_names_workbook(new_workbook_path, old_workbook_path)
#
# #TODO set these values
# new_workbook_path <- "C:\\Users\\dsap01\\Documents\\shsannualreport\\source\\column_and_variable_names\\variable_names.xlsx"
# old_workbook_path <- "C:\\Users\\dsap01\\Documents\\shsannualreport\\test_column_and_variable_names_to_update\\variable_names_to_update.xls"
# shs_update_names_workbook(new_workbook_path, old_workbook_path)
#
# #TODO set this value
# source_column_and_variable_names_path <- "C:\\Users\\dsap01\\Documents\\shsannualreport\\source\\column_and_variable_names"
# shs_extract_column_and_variable_names(source_column_and_variable_names_path, extracted_metadata_path)
# shs_process_column_names(extracted_data_path)
# shs_process_variable_names(extracted_data_path)
#
# shs_process_data(extracted_data_path)
