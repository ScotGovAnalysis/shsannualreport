#' Second part of SHS Data Explorer app processing
#'
#' \code{shs_app_processing_2} executes further data processing and creates an app that can be run/deployed.
#'
#' @param top_level_directory \code{string}.
#' The path of the directory the app was created in in \code{shs_app_processing_1}.
#' @param source_data_directory \code{string}.
#' The path of the directory containing source data and metadata.
#' @param reports_start_year \code{string}.
#' The first year available to select in the app's report generator.
#' @param reports_end_year \code{string}.
#' The final year available to select in the app's report generator.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_app_processing_2(top_level_directory, source_data_directory, reports_start_year, reports_end_year)
#' }
#'
#' @export

shs_app_processing_2 <- function(top_level_directory,
                                 source_data_directory,
                                 reports_start_year,
                                 reports_end_year) {

  app_directory <- file.path(top_level_directory, "ShsDataExplorer")
  app_data_directory <- file.path(app_directory, "data")
  app_dataset_directory <- file.path(app_data_directory, "dataset")
  app_metadata_directory <- file.path(app_data_directory, "metadata")
  app_source_directory <- file.path(app_directory, "source")
  app_reports_directory <- file.path(app_directory, "reports")
  app_www_directory <- file.path(app_directory, "www")

  column_names_save_file_path <- file.path(source_data_directory, "metadata", "column_names.xlsx")
  variable_names_save_file_path <- file.path(source_data_directory, "metadata", "variable_names.xlsx")

  dir.create(app_source_directory)
  dir.create(app_reports_directory)
  dir.create(app_www_directory)

  tryCatch({

    message(paste0("Extracting  ", column_names_save_file_path, " and ",
                   variable_names_save_file_path, " to ", app_metadata_directory))

    shsannualreport:::shs_extract_column_and_variable_names(
      app_metadata_directory = app_metadata_directory,
      column_names_save_file_path = column_names_save_file_path,
      variable_names_save_file_path = variable_names_save_file_path)

    message("Successfully extracted column and variable names")

  }, error = function(e) {

    message("Failed to extract column and variable names")

    stop(message(e))
  })

  tryCatch({

    message(paste0("Processing column names in ", app_dataset_directory))

    shsannualreport:::shs_process_column_names(app_dataset_directory = app_dataset_directory,
                                               app_metadata_directory = app_metadata_directory)

    message("Successfully processed column names")

  }, error = function(e) {

    message("Failed to process column names")

    stop(message(e))
  })

  tryCatch({

    message(paste0("Processing variable names in ", app_dataset_directory))

    shsannualreport:::shs_process_variable_names(app_dataset_directory = app_dataset_directory,
                                                 app_metadata_directory = app_metadata_directory)

    message("Successfully processed variable names")

  }, error = function(e) {

    message("Failed to process variable names")

    stop(message(e))
  })

  tryCatch({

    message(paste0("Removing local authority data from Scotland-only tables in ", app_dataset_directory))

    shsannualreport:::shs_clean_scotland_only_tables(app_dataset_directory = app_dataset_directory,
                                                     app_metadata_directory = app_metadata_directory)

    message("Successfully removed local authority data")

  }, error = function(e) {

    message("Failed to remove local authority data")

    stop(message(e))
  })

  tryCatch({

    message(paste0("Processing data in ", app_dataset_directory))

    shsannualreport:::shs_process_data(app_dataset_directory = app_dataset_directory,
                                       app_metadata_directory = app_metadata_directory)

    message("Successfully processed data")

  }, error = function(e) {

    message("Failed to process data")

    stop(message(e))
  })

  tryCatch({

    message(paste0("Creating variables source file in ", app_source_directory))

    shsannualreport:::shs_create_shiny_variables(reports_start_year = reports_start_year,
                                                 reports_end_year = reports_end_year,
                                                 app_source_directory = app_source_directory,
                                                 app_dataset_directory = app_dataset_directory,
                                                 app_metadata_directory = app_metadata_directory)

    message("Successfully created variables file")

  }, error = function(e) {

    message("Failed to create variables file")

    stop(message(e))
  })

  tryCatch({

    message(paste0("Creating reports files in ", app_reports_directory))

    shsannualreport:::shs_create_reports(app_dataset_directory = app_dataset_directory,
                                         app_metadata_directory = app_metadata_directory,
                                         app_reports_directory = app_reports_directory)

    message("Successfully created reports files")

  }, error = function(e) {

    message("Failed to create reports files")

    stop(message(e))
  })

  tryCatch({

    message(paste0("Copying necessary files to ", app_directory))

    shsannualreport:::shs_copy_app_files(app_directory = app_directory,
                                         app_source_directory = app_source_directory,
                                         app_www_directory = app_www_directory)

    message("Successfully copied files")

  }, error = function(e) {

    message("Failed to copy files")

    stop(message(e))
  })

  message("Processing completed successfully")

  message(paste0("The completed app can be run from ", app_directory))
}
