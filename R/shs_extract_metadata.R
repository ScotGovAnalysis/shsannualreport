#' Extract survey metadata from Excel
#'
#' \code{shs_extract_metadata} extracts survey metadata from Excel workbooks in a specified location,
#' and saves each sheet into a specified destination as an individual \code{.Rds} file.
#' Internal function for \code{shs_extract_data}.
#'
#' @param source_metadata_path \code{string}. The path of the directory containing survey metadata in Excel format.
#' @param extracted_metadata_path \code{string}. The path of the directory to be extracted to, created by \code{shs_extract_data}.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_extract_metadata(source_metadata_path, extracted_metadata_path)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_extract_metadata <- function(source_metadata_path,
                                 extracted_metadata_path) {

  files <- list.files(source_metadata_path)

  for (file in files) {

    workbook_path <- file.path(source_metadata_path, file)
    sheets <- readxl::excel_sheets(workbook_path)

    for (sheet in sheets) {

      df <- readxl::read_excel(workbook_path, sheet = sheet)
      saveRDS(df, file = file.path(extracted_metadata_path,
                                   paste0(sheet, ".Rds")))
    }
  }

  extracted_question_titles_path <- file.path(extracted_metadata_path, "question_titles.Rds")
  question_titles <- readRDS(extracted_question_titles_path)
  question_titles$TitleWithoutSpecialCharacter <- gsub("[[:punct:]]", "", question_titles$Title)
  saveRDS(question_titles, extracted_question_titles_path)
}
