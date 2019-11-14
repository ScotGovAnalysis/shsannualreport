#' Update column names from an extracted dataset
#'
#' \code{shs_update_column_names} updates column names in an  Excel spreadsheet with populated values from another spreadsheet.
#'
#' @param new_workbook_path \code{string}. The path of the file containing the new column names.
#' @param old_workbook_path \code{string}. The path of the file containing the previous diplay name of the column.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_update_column_names(new_workbook_path, old_workbook_path)
#'
#' @export

shs_update_column_names <- function(new_workbook_path, old_workbook_path) {

  new_column_names <- readxl::read_excel(new_workbook_path)
  old_column_names <- readxl::read_excel(old_workbook_path)

  new_column_names <- merge(x = new_column_names, y = old_column_names, by = "source_name", all.x = TRUE) %>%

    dplyr::select(source_name, display_name.y) %>%

    dplyr::rename(display_name = display_name.y)

  writexl::write_xlsx(new_column_names, path = new_workbook_path)

}
