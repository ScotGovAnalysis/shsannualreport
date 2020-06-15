# library(shsannualreport)
#
# source_data_directory <- "C:/Users/dsap01/Documents/shsannualreportdata"
#
# top_level_directory <- "C:/Users/dsap01/Documents"
#
# columns_to_remove <- c("sort", "_LABEL_", "var", "LABEL")
#
# reports_start_year <- 2013
# reports_end_year <- 2018
#
# top_level_directory <- file.path(top_level_directory, "SHS Data Explorer")
#
# shsannualreport::shs_app_processing_1(top_level_directory = top_level_directory,
#                                       columns_to_remove = columns_to_remove)
#
# shsannualreport::shs_app_processing_2(top_level_directory = top_level_directory,
#                                       reports_start_year = reports_start_year,
#                                       reports_end_year = reports_end_year)
