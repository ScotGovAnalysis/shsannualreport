#' Second part of SHS Data Explorer app processing
#'
#' \code{shs_create_app} creates an app that can be run/deployed, based on data created by
#' \code{shs_create_app_data}.
#'
#' @param destination_directory \code{string}.
#' The path of the directory to create the app in.
#' @param data_directory \code{string}.
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
#' shs_create_app(destination_directory, data_directory, reports_start_year, reports_end_year)
#' }
#'
#' @export

shs_create_app <- function(destination_directory,
                           data_directory,
                           reports_start_year,
                           reports_end_year) {

  app_directory <- file.path(destination_directory, "ShsDataExplorer")
  app_data_directory <- file.path(app_directory, "data")
  app_dataset_directory <- file.path(app_data_directory, "dataset")
  app_metadata_directory <- file.path(app_data_directory, "metadata")
  app_source_directory <- file.path(app_directory, "source")
  app_reports_directory <- file.path(app_directory, "reports")
  app_www_directory <- file.path(app_directory, "www")

  unlink(app_directory, recursive = TRUE)

  dir.create(app_directory)
  dir.create(app_data_directory)
  dir.create(app_dataset_directory)
  dir.create(app_metadata_directory)
  dir.create(app_source_directory)
  dir.create(app_reports_directory)
  dir.create(app_www_directory)

  tryCatch({

    message(paste0("Copying data files from ", data_directory))

    shsannualreport:::shs_copy_dataset_to_app(data_directory = data_directory,
                                              destination_data_directory = app_data_directory)

    cat(green("Successfully copied data files\n"))

  }, error = function(e) {

    cat(red("Failed to copy data files\n"))

    stop(message(e))
  })

  tryCatch({

    message(paste0("Creating variables source file in ", app_source_directory))

    shsannualreport:::shs_create_shiny_variables(reports_start_year = reports_start_year,
                                                 reports_end_year = reports_end_year,
                                                 app_source_directory = app_source_directory,
                                                 app_dataset_directory = app_dataset_directory,
                                                 app_metadata_directory = app_metadata_directory)

    cat(green("Successfully created variables file\n"))

  }, error = function(e) {

    cat(red("Failed to create variables file\n"))

    stop(message(e))
  })

  tryCatch({

    message(paste0("Copying necessary files to ", app_directory))

    shsannualreport:::shs_copy_app_files(app_directory = app_directory,
                                         app_source_directory = app_source_directory,
                                         app_www_directory = app_www_directory)

    cat(green("Successfully copied files\n"))

  }, error = function(e) {

    cat(red("Failed to copy files\n"))

    stop(message(e))
  })

  tryCatch({

    message(paste0("Creating reports files in ", app_reports_directory))

    shsannualreport:::shs_create_reports(app_dataset_directory = app_dataset_directory,
                                         app_metadata_directory = app_metadata_directory,
                                         app_reports_directory = app_reports_directory)

    cat(green("Successfully created reports files\n"))

  }, error = function(e) {

    cat(red("Failed to create reports files\n"))

    stop(message(e))
  })

  cat(green("\nProcessing completed successfully\n"))

  cat(blue("The completed app can be run from", app_directory, "\n"))
}
