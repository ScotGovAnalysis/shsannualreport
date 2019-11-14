#' Process SHS table of type 3
#'
#' \code{shs_process_table_type_3} cleans and formats data contained in an .Rds file in order to make the
#' data suitable for use in the SHS Annual Report.
#'
#' @param data_file_path \code{string}. The path to a file to be processed.
#' @param design_factors_path \code{string}. The path to a file containing design factor values.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_process_table_type_3(data_file_path, design_factors_path)
#'
#' @keywords internal
#'
#' @noRd

shs_process_table_type_3 <- function(data_file_path, design_factors_path) {

  #TODO - test setting this in shs_process_data and referring from there
  design <- readRDS(design_factors_path)

  tryCatch({
    df <- readRDS(data_file_path)
  }, error = function(cond) {
    message(paste0("Couldn't read file: ", data_file_path))
  })

  year <- paste0("20", sub(".*_ *(.*?) *.Rds*", "\\1", data_file_path))
  col_2_name <- names(df)[2]

  rename_columns <- colnames(df)[5:length(colnames(df)) - 2]
  colnames <- names(df)

  df$Year = year
  df <- df[, c("Year", colnames)]

  first_gather_column_index <- 4
  last_gather_column_index <- length(names(df)) - 2

  df <- df %>%
    tidyr::gather(key = GatherKey,
                  value = "Percent",
                  first_gather_column_index:last_gather_column_index) %>%
    dplyr::mutate(Percent = as.numeric(Percent), Base = as.numeric(Base)) %>%
    dplyr::group_by(Year, Council, GatherKey) %>%

    merge(design, by = "Year") %>%

    dplyr::mutate(sig_value = 1.96 * Factor * (sqrt((Percent / 100) * (1 - (Percent / 100)) / Base)),
                  LowerConfidenceLimit = round(Percent - (100 * sig_value), 2),
                  UpperConfidenceLimit = round(Percent + (100 * sig_value), 2),
                  Percent = round(Percent, 1)) %>%
    dplyr::ungroup()


  percent_values <- df %>%
    dplyr::select("Year", "Council", col_2_name, "GatherKey", "Percent", if ("All" %in% colnames(df)) {"All"}, "Base") %>%
    tidyr::spread("GatherKey", "Percent")

  sig_lower_string <- paste0("sig_lower_values <- df %>% dplyr::select(`Year`, `Council`, `",
                             col_2_name,
                             "`, `GatherKey`, `LowerConfidenceLimit`) %>% tidyr::spread(key = `GatherKey`, value = `LowerConfidenceLimit`) %>% dplyr::rename(")

  for (column_name in rename_columns) {
    sig_lower_string <- paste0(sig_lower_string, "`", column_name, "_l` = `", column_name, "`, ")
  }

  sig_lower_string <- (substr(sig_lower_string, 1, nchar(sig_lower_string) - 2)) %>%
    paste0(")")

  sig_upper_string <- paste0("sig_upper_values <- df %>% dplyr::select(`Year`, `Council`, `",
                             col_2_name,
                             "`, `GatherKey`, `UpperConfidenceLimit`) %>% tidyr::spread(key = `GatherKey`, value = `UpperConfidenceLimit`) %>% dplyr::rename(")

  for (column_name in rename_columns) {
    sig_upper_string <- paste0(sig_upper_string, "`", column_name, "_u` = `", column_name, "`, ")
  }

  sig_upper_string <- (substr(sig_upper_string, 1, nchar(sig_upper_string) - 2)) %>%
    paste0(")")

  eval(parse(text = sig_lower_string))
  eval(parse(text = sig_upper_string))
  bind_string <- paste0("df <- dplyr::bind_cols(c(percent_values, sig_lower_values, sig_upper_values)) %>%",
                        " dplyr::select(-Year1, -Council1, -`", col_2_name, "1`, -Year2, -Council2, -`", col_2_name, "2`)")
  eval(parse(text = bind_string))

  df
}
