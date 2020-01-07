#' Process time series data
#'
#' \code{shs_process_table_type_1} cleans and formats time series data contained in an .Rds file in order to make the
#' data suitable for use in the SHS Annual Report.
#'
#' @param data_file_path \code{string}. The path to a file to be processed.
#' @param design_factors_path \code{string}. The path to a file containing design factor values.
#' @param save_file_path \code{string}. The full path to save to, based on question title.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_process_table_type_1(data_file_path, save_file_path, design_factors_path)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_process_table_type_1 <- function(data_file_path, save_file_path, design_factors_path) {

# Read in files from parameters
df <- readRDS(data_file_path)
design <- readRDS(design_factors_path)

column_2_name <- colnames(df[2])

column_2_values <- unique(df[2])
column_2_values_string <- paste0("column_2_values$`", column_2_name, "`")
column_2_values <- eval(parse(text = column_2_values_string))

if ("All" %in% colnames(df)){
df <- subset(df, select=-c(All))
}

column_count <- length(colnames(df))
year_columns <- colnames(df[3:column_count])

main_df_string <- "df <- df %>% tidyr::gather(key = `Year`, value = `Percent`, "

for (year_column in year_columns) {
  main_df_string  <- paste0(main_df_string, "`", year_column, "`, ")
}

main_df_string <- (substr(main_df_string, 1, nchar(main_df_string) - 2)) %>%
  paste0(") %>% dplyr::mutate(`",
                        column_2_name,
                        "` = factor(`",
                        column_2_name,
                        "`, levels = c(")

for (column_2_value in column_2_values) {
  main_df_string <- paste0(main_df_string, "\"", column_2_value, "\", ")
}

main_df_string <- (substr(main_df_string, 1, nchar(main_df_string) - 2)) %>%

  paste0("))) %>% dplyr::group_by(Council, Year) %>% dplyr::mutate(Base = Percent[`",
                        column_2_name,
                        "` == 'Base']) %>% ",
                        "merge(design, by = 'Year') %>% ",
                        "dplyr::mutate(sig_value = 1.96 * as.numeric(Factor) * (sqrt((as.numeric(Percent) / 100) * (1 - (as.numeric(Percent) / 100)) / as.numeric(Base))), ",
                        "sig_lower = as.numeric(Percent) - (100 * sig_value), ",
                        "sig_lower = round(as.numeric(sig_lower), 1), ",
                        "sig_upper = as.numeric(Percent) + (100 * sig_value), ",
                        "sig_upper = round(as.numeric(sig_upper), 1), ",
                        "Percent = dplyr::if_else(Percent > 0, as.character(round(as.numeric(Percent), 1)), Percent) ",
                        ") %>% dplyr::ungroup()")

print(main_df_string)

values_df_string <- paste0("values_df <- df %>% select(`Council`, `", column_2_name, "`,`Year`, `Percent`) %>% tidyr::spread(key = `Year`, value = `Percent`)")

sig_lower_df_string <- paste0("sig_lower_df <- df %>% select(`Council`, `", column_2_name, "`,`Year`, `sig_lower`) %>% tidyr::spread(key = `Year`, value = `sig_lower`) %>% dplyr::rename(`Council_l` = `Council`, `", column_2_name, "_l` = `", column_2_name, "`,")

for (year_column in year_columns) {
  sig_lower_df_string  <- paste0(sig_lower_df_string, "`", year_column, "_l` = `", year_column, "`, ")
}

sig_lower_df_string <- (substr(sig_lower_df_string, 1, nchar(sig_lower_df_string) - 2)) %>%
  paste0(")")

sig_upper_df_string <- paste0("sig_upper_df <- df %>% select(`Council`, `", column_2_name, "`,`Year`, `sig_upper`) %>% tidyr::spread(key = `Year`, value = `sig_upper`) %>% dplyr::rename(`Council_u` = `Council`, `", column_2_name, "_u` = `", column_2_name, "`,")

for (year_column in year_columns) {
  sig_upper_df_string  <- paste0(sig_upper_df_string, "`", year_column, "_u` = `", year_column, "`, ")
}

sig_upper_df_string <- (substr(sig_upper_df_string, 1, nchar(sig_upper_df_string) - 2)) %>%
  paste0(")")

final_df_string <- paste0("df <- bind_cols(c(values_df, sig_lower_df, sig_upper_df)) %>% select(-Council_l, -Council_u, -`",
                          column_2_name,
                          "_l`, -`",
                          column_2_name,
                          "_u`)")

eval(parse(text = main_df_string))
eval(parse(text = values_df_string))
eval(parse(text = sig_lower_df_string))
eval(parse(text = sig_upper_df_string))
eval(parse(text = final_df_string))

saveRDS(df, save_file_path)
# file.remove(data_file_path)
}
