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
  row_order <- unique(df$temp_variable_name)

  colnames <- names(df)
  df$Year = year
  df <- df[, c("Year", colnames)]

  first_gather_column_index <- 4
  last_gather_column_index <- length(names(df))

  df <- tidyr::gather(df, key=gather_key, value=Percent, first_gather_column_index:last_gather_column_index) %>%
    dplyr::group_by(Council, Year, gather_key) %>%
    dplyr::mutate(Base = Percent[temp_variable_name == "Base"]) %>%
    merge(design, by = "Year") %>%
    dplyr::mutate(sig_value = 1.96 * as.numeric(Factor) * (sqrt((as.numeric(Percent) / 100) * (1 - (as.numeric(Percent) / 100)) / as.numeric(Base))),
                  sig_lower = as.numeric(Percent) - (100 * sig_value),
                  sig_lower = round(as.numeric(sig_lower), 1),
                  sig_upper = as.numeric(Percent) + (100 * sig_value),
                  sig_upper = round(as.numeric(sig_upper), 1),
                  Percent = dplyr::if_else(Percent > 0, as.character(round(as.numeric(Percent), 1)), as.character(Percent)) ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-6, -7, -8)

  percent_values <- df %>%
    dplyr::select("Year", "Council", "temp_variable_name", "gather_key", "Percent") %>%
    tidyr::spread(key = "gather_key", value = "Percent" )

  colnames(df)[3] <- col_2_name
  colnames(percent_values)[3] <- col_2_name

  sig_lower_string <- paste0("sig_lower_values <- df %>% dplyr::select(`Year`, `Council`, `",
                             col_2_name,
                             "`, `gather_key`, `sig_lower`) %>% tidyr::spread(key = `gather_key`, value = `sig_lower`) %>% dplyr::rename(`Year_l` = `Year`, `Council_l` = `Council`, ")

  for (column_name in rename_columns) {

    sig_lower_string <- paste0(sig_lower_string, "`", column_name, "_l` = `", column_name, "`, ")
  }

  sig_lower_string <- (substr(sig_lower_string, 1, nchar(sig_lower_string) - 2)) %>%
    paste0(")")

  sig_upper_string <- paste0("sig_upper_values <- df %>% dplyr::select(`Year`, `Council`, `",
                             col_2_name,
                             "`, `gather_key`, `sig_upper`) %>% tidyr::spread(key = `gather_key`, value = `sig_upper`) %>% dplyr::rename(`Year_u` = `Year`, `Council_u` = `Council`, ")

  for (column_name in rename_columns) {

    sig_upper_string <- paste0(sig_upper_string, "`", column_name, "_u` = `", column_name, "`, ")
  }

  sig_upper_string <- (substr(sig_upper_string, 1, nchar(sig_upper_string) - 2)) %>%
    paste0(")")

  bind_string <- paste0("df <- dplyr::bind_cols(c(percent_values, sig_lower_values, sig_upper_values)) %>%",
                        "dplyr::select(-Year_l, -Year_u, -Council_l, -Council_u, -`", col_2_name, "_l`, -`", col_2_name, "_u`)")

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

  get_order_string <- paste0("df$`", col_2_name, "`  <- factor(df$`", col_2_name, "`, levels = row_order)")

  reorder_string <- paste0("df <- df[order(df$`", col_2_name, "`),]")


  eval(parse(text = sig_lower_string))
  eval(parse(text = sig_upper_string))
  eval(parse(text = bind_string))
  eval(parse(text = select_string))
  eval(parse(text = get_order_string))
  eval(parse(text = reorder_string))

  df
}
