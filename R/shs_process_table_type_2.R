#' Process column percentage data
#'
#' \code{shs_process_table_type_2} cleans and formats column percentage data contained in an .Rds file in order to make the
#' data suitable for use in the SHS Annual Report.
#'
#' @param data_file_path \code{string}. The path to a file to be processed.
#' @param design_factors_path \code{string}. The path to a file containing design factor values.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_process_table_type_2(data_file_path, design_factors_path)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_process_table_type_2 <- function(data_file_path, design_factors_path) {

  tryCatch({
  df <- readRDS(data_file_path)
  }, error = function(cond) {
    message(paste0("Couldn't read file: ", data_file_path))})

  if ("All" %in% colnames(df)){

    df <- subset(df, select=-c(All))
  }

  design <- readRDS(design_factors_path)

  year <- paste0("20", sub(".*_ *(.*?) *.Rds*", "\\1", data_file_path))
  rename_columns <- colnames(df)[2:length(colnames(df))]
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
    dplyr::mutate(Base = Percent[temp_variable_name == "Base"]) %>%
    merge(design, by = "Year") %>%
    dplyr::mutate(sig_value = 1.96 * Factor * (sqrt((Percent / 100) * (1 - (Percent / 100)) / Base)),
                  LowerConfidenceLimit = round(Percent - (100 * sig_value), 2),
                  UpperConfidenceLimit = round(Percent + (100 * sig_value), 2),
                  Percent = round(Percent, 1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-6, -7, -8)

  names(df)[3] <- col_2_name

  percent_values <- df %>%
    select("Year", "Council", col_2_name, "gather_key", "Percent") %>%
    spread(key = "gather_key", value = "Percent" )

  sig_lower_string <- paste0("sig_lower_values <- df %>% select(`Year`, `Council`, `",
                             col_2_name,
                             "`, `gather_key`, `LowerConfidenceLimit`) %>% spread(key = `gather_key`, value = `LowerConfidenceLimit`) %>% rename(`Year_l` = `Year`, `Council_l` = `Council`, ")

  for (column_name in rename_columns) {

    sig_lower_string <- paste0(sig_lower_string, "`", column_name, "_l` = `", column_name, "`, ")
  }

  sig_lower_string <- (substr(sig_lower_string, 1, nchar(sig_lower_string) - 2)) %>%
    paste0(")")

  sig_upper_string <- paste0("sig_upper_values <- df %>% select(`Year`, `Council`, `",
                             col_2_name,
                             "`, `gather_key`, `UpperConfidenceLimit`) %>% spread(key = `gather_key`, value = `UpperConfidenceLimit`) %>% rename(`Year_u` = `Year`, `Council_u` = `Council`, ")

  for (column_name in rename_columns) {

    sig_upper_string <- paste0(sig_upper_string, "`", column_name, "_u` = `", column_name, "`, ")
  }

  sig_upper_string <- (substr(sig_upper_string, 1, nchar(sig_upper_string) - 2)) %>%
    paste0(")")

  bind_string <- paste0("df <- bind_cols(c(percent_values, sig_lower_values, sig_upper_values)) %>%",
                        "select(-Year_l, -Year_u, -Council_l, -Council_u, -`", col_2_name, "_l`, -`", col_2_name, "_u`)")

  select_string <- "df <- dplyr::select(df, Year, Council, "

  for (column_name in rename_columns) {

    select_string <- paste0(select_string, "`", column_name, "`, ")
  }

  for (column_name in rename_columns[2:length(rename_columns)]) {

    select_string <- paste0(select_string, "`", column_name, "_l`, ")
  }

  for (column_name in rename_columns[2:length(rename_columns)]) {

    select_string <- paste0(select_string, "`", column_name, "_u`, ")
  }

  select_string <- (substr(select_string, 1, nchar(select_string) - 2)) %>%
    paste0(")")

  eval(parse(text = sig_lower_string))
  eval(parse(text = sig_upper_string))
  eval(parse(text = bind_string))
  eval(parse(text = select_string))

  df
}
