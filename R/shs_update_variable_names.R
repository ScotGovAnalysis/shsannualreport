#' Update variable names from an extracted dataset
#'
#' \code{shs_update_variable_names} updates variable names in an  Excel spreadsheet with populated values from another spreadsheet.
#'
#' @param new_workbook_path \code{string}. The path of the file containing the new variable names.
#' @param old_workbook_path \code{string}. The path of the file containing the previous diplay name of the variable.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_update_variable_names(new_workbook_path, old_workbook_path)
#'
#' @export

shs_update_variable_names <- function(new_workbook_path, old_workbook_path) {

  new_variable_names <- readxl::read_excel(new_workbook_path)
  old_variable_names <- readxl::read_excel(old_workbook_path)

  new_variable_names <- merge(x = new_variable_names, y = old_variable_names, by = "source_name", all.x = TRUE) %>%

    dplyr::select(source_name, display_name.y) %>%

    dplyr::rename(display_name = display_name.y)

  writexl::write_xlsx(new_variable_names, path = new_workbook_path)

}
