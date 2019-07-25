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

shs_process_combine_multiple_years <- function(extracted_dataset_path) {

  # List all chapters in data directory
  chapters <- list.files(extracted_dataset_path)

  # Loop through chapters
  for (chapter in chapters) {

    # List tables in chapter
    chapter_path <- file.path(extracted_dataset_path, chapter)
    tables <- list.files(chapter_path)

    #Loop through tables
    for (table in tables){

    }
  }

}
