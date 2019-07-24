#' Rename column names in extracted datasets
#'
#' \code{shs_rename_columns} renames columns in extracted datasets according to data specified \code{column_names.Rds} in extracted metadata.
#'
#' @param extracted_dataset_path \code{string}. The path of the directory containing extracted survey data.
#' @param extracted_metadata_path \code{string}. The path of the directory containing extracted metadata.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_rename_columns(extracted_dataset_path, extracted_metadata_path)
#'
#' @export

shs_rename_columns <- function(extracted_dataset_path,
                               extracted_metadata_path) {

  # Load in column_names reference data
  column_reference <- readRDS(paste0(extracted_metadata_path,
                                     "\\column_names.Rds"))

  # Replace any NA values in display_name with source_name
  column_reference$display_name[is.na(column_reference$display_name)] <-
    column_reference$source_name[is.na(column_reference$display_name)]

  # List all chapted in data directory
  chapters <- list.files(extracted_dataset_path)

  # Loop through chapters
  for (chapter in chapters) {

    # List tables in chapter
    chapter_path <- paste0(extracted_dataset_path, "\\", chapter)
    tables <- list.files(chapter_path)

    #Loop through tables
    for (table in tables){

      # Read in table and extract column names
      table_path <- paste0(extracted_dataset_path, "\\", chapter, "\\", table)
      df <- readRDS(table_path)
      column_names <- colnames(df)

      #Loop through column names
      for (column_name in column_names) {

        # Get new column name from reference table, update old column name, and save
        new_column_name <- column_reference[column_reference$source_name
                                            == column_name, 2]

        colnames(df)[colnames(df) == column_name] <- new_column_name

        saveRDS(df, file = paste0(extracted_dataset_path,
                                  "\\", chapter, "\\", table))
      }
    }
  }
}
