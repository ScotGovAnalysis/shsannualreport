# library(shsannualreport)

source_dataset_directory <- "C:/Users/dsap01/Downloads/data/dataset"
destination_directory <- "C:/Users/dsap01/Desktop"
columns_to_remove <- c("sort", "_LABEL_", "var", "LABEL")
existing_column_names_path <- "C:/Users/dsap01/Downloads/data/metadata/old_column_names.xlsx"
existing_variable_names_path <- "C:/Users/dsap01/Downloads/data/metadata/old_variable_names.xlsx"


shsannualreport::shs_create_names_workbooks(destination_directory = destination_directory,
                                            source_dataset_directory = source_dataset_directory,
                                            existing_column_names_path = existing_column_names_path,
                                            existing_variable_names_path = existing_variable_names_path,
                                            columns_to_remove = columns_to_remove)

destination_directory <- "C:/Users/dsap01/Documents"
source_data_directory <- "C:/Users/dsap01/Downloads/data"
column_names_workbook_path <- "C:/Users/dsap01/Downloads/data/metadata/old_column_names.xlsx"
variable_names_workbook_path <- "C:/Users/dsap01/Downloads/data/metadata/old_variable_names.xlsx"
columns_to_remove <- c("sort", "_LABEL_", "var", "LABEL")

shsannualreport::shs_create_app_data(destination_directory = destination_directory,
                                     source_data_directory = source_data_directory,
                                     columns_to_remove = columns_to_remove,
                                     column_names_workbook_path = column_names_workbook_path,
                                     variable_names_workbook_path = variable_names_workbook_path)

destination_directory <- "C:/Users/dsap01/Desktop"
data_directory <- "C:/Users/dsap01/Documents/data"
reports_start_year <- 2013
reports_end_year <- 2019

shsannualreport::shs_create_app(destination_directory = destination_directory,
                                data_directory = data_directory,
                                reports_start_year = reports_start_year,
                                reports_end_year = reports_end_year)

