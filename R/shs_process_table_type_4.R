#' Process SHS table of type 4
#'
#' \code{shs_process_table_type_4} cleans and formats data contained in an .Rds file in order to make the
#' data suitable for use in the SHS Annual Report.
#'
#' @param data_file_path \code{string}. The path to a file to be processed.
#' @param design_factors_path \code{string}. The path to a file containing design factor values.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_process_table_type_4(data_file_path, design_factors_path)
#'
#' @keywords internal
#'
#' @noRd

shs_process_table_type_4 <- function(data_file_path, save_file_path) {

  tryCatch({
    df <- readRDS(data_file_path)
  }, error = function(cond) {
    message(paste0("Couldn't read file: ", data_file_path))
  })

  saveRDS(df, save_file_path)
  file.remove(data_file_path)
}
