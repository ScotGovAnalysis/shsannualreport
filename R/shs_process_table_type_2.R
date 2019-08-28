#' Process SHS table of type 2
#'
#' \code{shs_process_table_type_2} cleans and formats data contained in an .Rds file in order to make the
#' data suitable for use in the SHS Annual Report.
#'
#' @param data_file_path \code{string}. The path to a file to be processed.
#' @param design_factors_path \code{string}. The path to a file containing design factor values.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_process_table_type_2(data_file_path, design_factors_path)
#'
#' @keywords internal
#'
#' @noRd

shs_process_table_type_2 <- function(data_file_path, design_factors_path) {

  df <- readRDS(data_file_path)
  year <- sub(".*X_ *(.*?)", "\\1", colnames(df[3]))

  df$"Year" <- year
  names(df)[3] <- "Percent"
  # TODO: Remove when renaming completed - figure out how I borke it too
  # names(df)[2] <- "temp_variable_name"
  df <- df[,-4]

  ###
  # TODO - figure out how to round and add statistical significance
  # df %>%  dplyr::mutate(Percent = as.numeric(Percent)) %>%
  #   dplyr::group_by(Council, Year) %>%
  #   dplyr::mutate(n = Percent[temp_variable_name == "Base"]) %>%
  #   merge(design, by = "Year") %>%
  #   dplyr::mutate(sig_value = 1.96 * Factor * (sqrt((Percent / 100) * (1 - (Percent / 100)) / n)),
  #                 LowerConfidenceLimit = round(Percent - (100 * sig_value), 2),
  #                 UpperConfidenceLimit = round(Percent + (100 * sig_value), 2),
  #                 Percent = round(Percent, 1)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(-6, -7, -8)
  ###

  df
}
