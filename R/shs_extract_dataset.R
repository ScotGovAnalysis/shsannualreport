#' Extract an SHS dataset from Excel workbooks
#'
#' \code{shs_extract_dataset} extracts raw survey data from Excel workbooks in a specified location,
#' and saves each sheet into a specified destination as an individual \code{.Rds} file.
#' Internal function for \code{shs_extract_data}.
#'
#' @param source_dataset_directory \code{string}.
#' The path of the directory containing survey data in Excel format.
#' @param dataset_directory \code{string}.
#' The path of the app directory to save the dataset to.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_extract_dataset(source_dataset_directory, dataset_directory)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_extract_dataset <- function(source_dataset_directory, dataset_directory) {

  files <- list.files(source_dataset_directory)

  for (file in files) {

    workbook_path <- file.path(source_dataset_directory, file)
    sheets <- readxl::excel_sheets(workbook_path)

    for (sheet in sheets) {

      if (grepl("TAB", sheet)) {

        chapter_number <- sub(".*FINAL_C *(.*?) *_TAB.*", "\\1", sheet)

        tab_number <- sub(".*_TAB *(.*?)", "\\1", sheet)

        if (substring(tab_number, 1, 1) == "0") {

          tab_number <- substring(tab_number, 2)
        }

        dataframe_id <- paste0("Table ", chapter_number, ".", tab_number)

      } else if (grepl("FIG", sheet)) {

        chapter_number <- sub(".*FINAL_C *(.*?) *_FIG.*", "\\1", sheet)

        fig_number <- sub(".*_FIG *(.*?)", "\\1", sheet)

        if (substring(fig_number, 1, 1) == "0") {

          fig_number <- substring(fig_number, 2)
        }

        dataframe_id <- paste0("Figure ", chapter_number, ".", fig_number)

      } else {

        stop(paste0("Unknown type of data in sheet ", sheet, ".
                    Only 'FIG' or 'TAB' sheets permitted."))
      }

      df <- readxl::read_excel(workbook_path, sheet = sheet)

      saveRDS(df, file = file.path(dataset_directory,
                                   paste0(dataframe_id, ".Rds")))
    }
  }
}
