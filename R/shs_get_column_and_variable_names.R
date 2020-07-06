#' Extract column and variable names from raw data
#'
#' \code{shs_get_column_and_variable_names} extracts column and variable names from an SHS survey dataset,
#' and writes the output to Excel.
#' The output file 'column_names.xlsx' will contain all column names in the source files, minus any specified in
#' the \code{column_names_to_exclude} argument.
#' The output file 'variable_names.xlsx' will contain all unique values present in the second column of all tables in the raw data,
#' once columns names in \code{column_names_to_exclude} are removed.
#' The resulting Excel sheets will both have two columns: source_name and display_name. The display name value is the value
#' that will be displayed in the final Shiny app, these values can be added manually or by providing paths of existing,
#' populated sheets to the arguments \code{existing_column_names_path} and \code{existing_variable_names_path}.
#'
#' @param top_level_directory \code{string}.
#' The path of the directory the app will be created in.
#' @param source_dataset_directory \code{string}.
#' The path of the directory containing source data and metadata.
#' @param columns_to_remove \code{string}.
#' Unnecessary columns to remove from the dataset. If incorrect values provided, output file variable_names.xlsx will be populated incorrectly.
#' @param existing_column_names_path \code{string}.
#' A path to an existing Excel file with the same structure. Any display_name values will be copied to the new output file.
#' @param existing_variable_names_path \code{string}.
#' A path to an existing Excel file with the same structure. Any display_name values will be copied to the new output file.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_get_column_and_variable_names(top_level_directory, source_dataset_directory, columns_to_remove, existing_column_names_path, existing_variable_names_path)
#' }
#'
#' @export

shs_get_column_and_variable_names <- function(destination_directory,
                                              source_dataset_directory,
                                              columns_to_remove,
                                              existing_column_names_path = NULL,
                                              existing_variable_names_path = NULL) {

  column_names_save_file_path <- file.path(destination_directory, "column_names.xlsx")
  variable_names_save_file_path <- file.path(destination_directory, "variable_names.xlsx")

  tryCatch({

    message(paste0("Extracting column and variable names from ", source_dataset_directory, " to ",
                   destination_directory))

    shsannualreport:::shs_create_names_workbooks(source_dataset_directory = source_dataset_directory,
                                                 destination_directory = destination_directory,
                                                 columns_to_remove = columns_to_remove)

    cat(green("Successfully wrote files 'columns_names.xlsx' and 'variable_names.xlsx'\n"))

  }, error = function(e) {

    cat(red("Failed to write files 'columns_names.xlsx' and 'variable_names.xlsx'\n"))

    stop(message(e))
  })


  if (is.null(existing_column_names_path)) {

    message("No value was given for the argument 'existing_column_names'; column_names.xlsx has not been updated")

  } else if (file.exists(existing_column_names_path)) {

    tryCatch({

      message(paste0("Writing column names present in ", existing_column_names_path,
                     " to ", column_names_save_file_path))

      shsannualreport:::shs_update_names_workbook(new_workbook_path = column_names_save_file_path,
                                                  old_workbook_path = existing_column_names_path)

      cat(green("Successfully wrote column names\n"))

    }, error = function(e) {

      cat(red("Failed to write column names\n"))

      stop(message(e))
    })

  } else {

    message(cat(red("The value given for 'existing_column_names_path' does not exist; ",
                    column_names_save_file_path, " has not been updated.")))

    message(cat(red("The value given was ", existing_column_names_path)))
  }


  if (is.null(existing_variable_names_path)) {

    message("No value was given for the argument 'existing_variable_names'; variable_names.xlsx has not been updated")

  } else if (file.exists(existing_variable_names_path)) {

    tryCatch({

      message(paste0("Writing variable names present in ", existing_variable_names_path,
                     " to ", variable_names_save_file_path))

      shsannualreport:::shs_update_names_workbook(new_workbook_path = variable_names_save_file_path,
                                                  old_workbook_path = existing_variable_names_path)

      cat(green("Successfully wrote variable names\n"))

    }, error = function(e) {

      cat(red("Failed to write variable names\n"))

      stop(message(e))
    })

  } else {

    message(cat(red("The value given for 'existing_variable_names_path' does not exist; ",
                    variable_names_save_file_path, " has not been updated.")))

    message(cat(red("The value given was ", existing_variable_names_path)))
  }
}
