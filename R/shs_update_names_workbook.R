#' Update workbook of variable names from an extracted dataset
#'
#' \code{shs_update_names_workbook} updates column or variable names in an
#' Excel spreadsheet with populated values from another spreadsheet.
#'
#' @param new_workbook_path \code{string}.
#' The path of the file containing the new variable names.
#' @param old_workbook_path \code{string}.
#' The path of the file containing the previous diplay name of the variable.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_update_names_workbook(new_workbook_path, old_workbook_path)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_update_names_workbook <- function(new_workbook_path, old_workbook_path) {

  new_names_df <- readxl::read_excel(new_workbook_path)
  old_names_df <- readxl::read_excel(old_workbook_path)

  sheet <- readxl::excel_sheets(new_workbook_path)

  new_names_df <- merge(x = new_names_df, y = old_names_df, by = "source_name", all.x = TRUE) %>%

    dplyr::select(source_name, display_name.y) %>%

    dplyr::rename(display_name = display_name.y)

  writeexl_string <- paste0("writexl::write_xlsx(list(", sheet, " = new_names_df), path = new_workbook_path)")

  eval(parse(text = writeexl_string))

}
