#' Extract chapter and question titles from Excel
#'
#' \code{shs_extract_titles} extracts data from Excel workbooks in a specified location,
#' and saves all sheets as .Rds files in a newly created location.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_extract_titles()
#'
#' @keywords internal
#'
#' @noRd

shs_extract_titles <- function(source_titles_path, extracted_titles_data_path) {
  workbook_path <- file.path(source_titles_path, "SHS2017_chapter_and_question_titles.xls")
  workbook <- XLConnect::loadWorkbook(workbook_path)
  sheets <- readxl::excel_sheets(workbook_path)
  for (sheet in sheets) {
    df <- XLConnect::readWorksheet(workbook, sheet = sheet, header = TRUE)
    saveRDS(df, file = file.path(extracted_titles_data_path, paste0(sheet, ".Rds")))
  }
}
