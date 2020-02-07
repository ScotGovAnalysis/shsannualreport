#' Process SHS manual input data tables
#'
#' \code{shs_process_table_type_4} cleans and formats manually input data contained in an .Rds file in order to make the
#' data suitable for use in the SHS Annual Report.
#'
#' @param data_file_path \code{string}. The path to a file to be processed.
#' @param design_factors_path \code{string}. The path to a file containing design factor values.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_process_table_type_4(data_file_path, design_factors_path)
#' }
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

  column_names <- colnames(df)
  first_column_to_round <- 4
  last_column_to_round <- length(column_names)

  columns_to_round <- column_names[first_column_to_round:last_column_to_round]

  round_string <- paste0("df <- df %>% dplyr::mutate(")

  for (column_name in columns_to_round) {

    round_string <- paste0(round_string, "`", column_name, "` = as.numeric(`", column_name, "`), ")

  }

  round_string <- (substr(round_string, 1, nchar(round_string) - 2))

  round_string <- paste0(round_string, ") %>% mutate(")

  for (column_name in columns_to_round) {

    round_string <- paste0(round_string, "`", column_name, "` = round(`", column_name, "`, 0), ")

  }

  round_string <- (substr(round_string, 1, nchar(round_string) - 2))

  round_string <- (paste0(round_string, ")"))

  eval(parse(text = round_string))

  file.remove(data_file_path)
  saveRDS(df, save_file_path)
}
