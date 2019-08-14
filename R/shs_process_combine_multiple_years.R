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
#' shs_process_combine_multiple_years(extracted_dataset_path)
#'
#' @keywords internal
#'
#' @noRd


#TODO: add years and figure out how to combine them using rbind (i.e. why are headers different and which one to choose)

shs_process_combine_multiple_years <- function(extracted_dataset_path) {

  # List all files in data directory
  files <- list.files(extracted_dataset_path)

  # Set up list for files split by year
  files_with_years <- list()

    #Loop through files to get list of files containing a single year's data
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
    dataframe_by_year <- data.frame()

    # Loop through each year of split file
    for (df in dfs) {
      year <- sub(".*_ *(.*?) *.Rds", "\\1", df)
      df_path <- file.path(extracted_dataset_path, df)
      data <- readRDS(df_path)
      data$Year <- year
      dataframe_by_year <- tryCatch({rbind(dataframe_by_year, data)
      }, error = function(e) {
        print(paste0("error: ", e, " dataframe: ", df_path))})
      # file.remove(df_path)
    }

    assign(file, dataframe_by_year)

  }
}

