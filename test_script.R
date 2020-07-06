# library(shsannualreport)
#
source_dataset_directory <- "C:/Users/dsap01/Downloads/data/dataset"
destination_directory <- "C:/Users/dsap01/Desktop"
columns_to_remove <- c("sort", "_LABEL_", "var", "LABEL")
existing_column_names_path <- "C:/Users/dsap01/Downloads/data/metadata/old_column_names.xlsx"
existing_variable_names_path <- "C:/Users/dsap01/Downloads/data/metadata/old_variable_names.xlsx"


shsannualreport::shs_get_column_and_variable_names(destination_directory = destination_directory,
                                                   source_dataset_directory = source_dataset_directory,
                                                   columns_to_remove = columns_to_remove,
                                                   existing_column_names_path = existing_column_names_path,
                                                   existing_variable_names_path = existing_variable_names_path)
#
# source_data_directory <- "C:/Users/dsap01/Downloads/data"
#
# top_level_directory <- "C:/Users/dsap01/Documents"
#
# columns_to_remove <- c("sort", "_LABEL_", "var", "LABEL")
#
# reports_start_year <- 2013
# reports_end_year <- 2019
#
# shsannualreport::shs_app_processing_1(top_level_directory = top_level_directory,
#                                       source_data_directory = source_data_directory,
#                                       columns_to_remove = columns_to_remove)
#
# shsannualreport::shs_app_processing_2(top_level_directory = top_level_directory,
#                                       source_data_directory = source_data_directory,
#                                       reports_start_year = reports_start_year,
#                                       reports_end_year = reports_end_year)
#
