#' Process SHS survey Excel dataset, output to Rds files for use in Shiny app
#'
#' \code{shs_create_app_data} extracts data and metadata, removes unnecessary columns,
#' renames columns and variables, processes data into the correct format, and removes local
#' authority data from Scotland-only tables.
#'
#' @param destination_directory \code{string}.
#' The path of the directory to save the output data to.
#' @param source_data_directory \code{string}.
#' The path of the directory containing source data and metadata in Excel format.
#' @param columns_to_remove \code{string}.
#' Unnecessary columns to remove from the dataset.
#' @param column_names_workbook_path \code{string}.
#' The path of an Excel file containing column renaming details.
#' @param variable_names_workbook_path \code{string}.
#' The path of an Excel file containing variable renaming details.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_create_app_data(destination_directory, source_data_directory, column_names_workbook_path, variable_names_workbook_path, columns_to_remove)
#' }
#'
#' @export

shs_create_app_data <- function(destination_directory,
                                source_data_directory,
                                columns_to_remove,
                                column_names_workbook_path,
                                variable_names_workbook_path) {

  source_dataset_directory <- file.path(source_data_directory, "dataset")
  source_metadata_directory <- file.path(source_data_directory, "metadata")

  data_directory <- file.path(destination_directory, "data")
  dataset_directory <- file.path(data_directory, "dataset")
  metadata_directory <- file.path(data_directory, "metadata")

  column_names_workbook_path <- file.path(source_metadata_directory, "column_names.xlsx")
  variable_names_workbook_path <- file.path(source_metadata_directory, "variable_names.xlsx")

  unlink(dataset_directory, recursive = TRUE)
  unlink(metadata_directory, recursive = TRUE)

  dir.create(data_directory)
  dir.create(dataset_directory)
  dir.create(metadata_directory)

  tryCatch({

    message(paste0("Extracting metadata from ", source_metadata_directory, " to ", metadata_directory))

    shsannualreport:::shs_extract_metadata(source_metadata_directory = source_metadata_directory,
                                           metadata_directory = metadata_directory)

    cat(green("Successfully extracted metadata\n"))

  }, error = function(e) {

    cat(red("Failed to extract metadata\n"))

    stop(message(e))
  })

  tryCatch({

    message(paste0("Extracting dataset from ", source_dataset_directory, " to ", dataset_directory))

    shsannualreport:::shs_extract_dataset(source_dataset_directory = source_dataset_directory,
                                          dataset_directory = dataset_directory)

    cat(green("Successfully extracted dataset\n"))

  }, error = function(e) {

    cat(red("Failed to extract data\n"))

    stop(message(e))
  })

  tryCatch({

    message(paste0("Removing columns ", paste(columns_to_remove, collapse = ", "),
                   " from data in ", dataset_directory))

    shsannualreport:::shs_remove_columns(dataset_directory = dataset_directory,
                                         columns_to_remove = columns_to_remove)

    cat(green("Successfully removed columns\n"))

  }, error = function(e) {

    cat(red("Failed to remove columns\n"))

    stop(message(e))
  })

  tryCatch({

    message(paste0("Extracting  ", column_names_workbook_path, " and ",
                   variable_names_workbook_path, " to ", metadata_directory))

    shsannualreport:::shs_extract_column_and_variable_names(metadata_directory = metadata_directory,
                                                            column_names_workbook_path = column_names_workbook_path,
                                                            variable_names_workbook_path = variable_names_workbook_path)

    cat(green("Successfully extracted column and variable names\n"))

  }, error = function(e) {

    cat(red("Failed to extract column and variable names\n"))

    stop(message(e))
  })

  tryCatch({

    message(paste0("Processing column names in ", dataset_directory))

    shsannualreport:::shs_process_column_names(dataset_directory = dataset_directory,
                                               metadata_directory = metadata_directory)

    cat(green("Successfully processed column names\n"))

  }, error = function(e) {

    cat(red("Failed to process column names\n"))

    stop(message(e))
  })

  tryCatch({

    message(paste0("Processing variable names in ", dataset_directory))

    shsannualreport:::shs_process_variable_names(dataset_directory = dataset_directory,
                                                 metadata_directory = metadata_directory)

    cat(green("Successfully processed variable names\n"))

  }, error = function(e) {

    cat(red("Failed to process variable names\n"))

    stop(message(e))
  })

  tryCatch({

    message(paste0("Processing data in ", dataset_directory))

    shsannualreport:::shs_process_dataset(dataset_directory = dataset_directory,
                                          metadata_directory = metadata_directory)

    cat(green("Successfully processed data\n"))

  }, error = function(e) {

    cat(red("Failed to process data\n"))

    stop(message(e))
  })

  tryCatch({

    message(paste0("Removing local authority data from Scotland-only tables in ", dataset_directory))

    shsannualreport:::shs_clean_scotland_only_tables(dataset_directory = dataset_directory,
                                                     metadata_directory = metadata_directory)

    cat(green("Successfully removed local authority data\n"))

  }, error = function(e) {

    cat(red("Failed to remove local authority data\n"))

    stop(message(e))
  })

  cat(green("\nProcessing completed successfully\n"))
}
