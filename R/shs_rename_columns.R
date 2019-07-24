#' Rename column names in Rda files
#'
#' \code{shs_rename_columns} renames column names in Rda files in extracted datasets according to data specified in extracted metadata.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_rename_columns()
#'
#' @export

# Set up input and output directories
shs_rename_columns <- function(extracted_dataset_path, extracted_metadata_path) {

  column_reference <- readRDS(paste0(extracted_metadata_path, "\\column_names.Rds"))
  column_reference$display_name[is.na(column_reference$display_name)] <- column_reference$source_name[is.na(column_reference$display_name)]
  chapters <- list.files(extracted_dataset_path)

  for (chapter in chapters) {

    chapter_path <- paste0(extracted_dataset_path, "\\", chapter)

    tables <- list.files(chapter_path)

    for (table in tables){

      table_path <- paste0(extracted_dataset_path, "\\", chapter, "\\", table)

      df <- readRDS(table_path)

      column_names <- colnames(df)

      for (column_name in column_names) {

          new_column_name <- column_reference[column_reference$source_name == column_name, 2]

          colnames(df)[colnames(df)==column_name] <- new_column_name

          saveRDS(df, file = paste0(extracted_dataset_path, "\\", chapter, "\\", table))
      }
    }
  }
}
