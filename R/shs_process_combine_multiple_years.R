#' Combine data for multiple years into one table
#'
#' \code{shs_process_combine_multiple_years} combines extracted SHS survey data for a figure or table which is split over multiple years into a single dataframe.
#' The new dataframe is saved to an .Rds file, and the original .Rds files are deleted.
#'
#' @param extracted_dataset_path \code{string}. The path of the directory a dataset has previously been extracted to, by \code{shs_extract_data}.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_process_combine_multiple_years(extracted_folder_path)
#'
#' @export

#TODO: add years and figure out how to combine them using rbind (i.e. why are headers differenct and which one to choose)

shs_process_combine_multiple_years <- function(extracted_dataset_path) {

  # List all files in data directory
  files <- list.files(extracted_dataset_path)

  # Set up list for files split by year
  files_with_years <- list()

    #Loop through files
    for (file in files){

      #Get full file path
      file_path <- file.path(extracted_dataset_path, file)

      #Get final two characters of file name
      final_chars <- substr(file_path, nchar(file_path)-5, nchar(file_path))
      final_two_chars <- sub(".Rds", "", final_chars)

      # Check that final two characters are digits (i.e. years)
      if (grepl("^[[:digit:]]", final_two_chars) == TRUE) {

        # Get name of dataset without year
        dataframe_name <- sub("*(.*?) *_.*", "\\1", file)

        # Add dataset to list of datasets split by year
        files_with_years <- c(files_with_years, dataframe_name)
      }
    }

  # Remove duplicates from list
  files_with_years <- unique(files_with_years)

  # Loop through files split by year
  for (file in files_with_years) {

    #Create list of files split by year
    dfs <- list.files(extracted_dataset_path, pattern = file)

    # Create empty dataframe for all years
    dataframe_by_year <- list()

    # Loop through each year of split file
    for (df in dfs) {
      df <- (file.path(extracted_dataset_path, df))
      dataframe_by_year <- c(dataframe_by_year, df)
      # lapply(dataframes_by_year, readRDS)

    }

    dataframes_by_year <- do.call(rbind, lapply(file.path(dataframe_by_year), readRDS, header=TRUE))
    assign(file, dataframes_by_year)

    # set up first dataframe to have year as value
    # set up later dataframes to add columns and year
  }
}

