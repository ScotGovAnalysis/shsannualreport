#' Process SHS table of type 3
#'
#' \code{shs_process_table_type_3} cleans and formats data contained in an .Rds file in order to make the
#' data suitable for use in the SHS Annual Report.
#'
#' @param data_file_path \code{string}. The path to a file to be processed.
#' @param design_factors_path \code{string}. The path to a file containing design factor values.
#' @param gather_key \code{string}. The name of the column to gather under.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_process_table_type_3(data_file_path, design_factors_path)
#'
#' @keywords internal
#'
#' @noRd

shs_process_table_type_3 <- function(data_file_path, design_factors_path, gather_key) {
  df <- readRDS(data_file_path)
  design <- readRDS(design_factors_path)
  year <- paste0("20", sub(".*_ *(.*?) *.Rds*", "\\1", data_file_path))
  col_2_name <- names(df)[2]
  names(df)[2] <- "temp_variable_name"

  colnames <- names(df)
  df$Year = year
  df <- df[, c("Year", colnames)]

  first_gather_column_index <- 4
  last_gather_column_index <- length(names(df))

  df <- tidyr::gather(df, key=gather_key, value=Percent, first_gather_column_index:last_gather_column_index) %>%
    dplyr::mutate(Percent = as.numeric(Percent)) %>%
    dplyr::group_by(Council, Year, gather_key) %>%
    dplyr::mutate(n = Percent[temp_variable_name == "Base"]) %>%
    merge(design, by = "Year") %>%
    dplyr::mutate(sig_value = 1.96 * Factor * (sqrt((Percent / 100) * (1 - (Percent / 100)) / n)),
                  LowerConfidenceLimit = round(Percent - (100 * sig_value), 2),
                  UpperConfidenceLimit = round(Percent + (100 * sig_value), 2),
                  Percent = round(Percent, 1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-6, -7, -8)

  names(df)[3] <- col_2_name
  names(df)[4] <- gather_key

  df
}
