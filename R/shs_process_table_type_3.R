#' Process row percentage data
#'
#' \code{shs_process_table_type_3} cleans and formats row percentage data contained in an .Rds file in order to make the
#' data suitable for use in the SHS Annual Report.
#'
#' @param data_file_path \code{string}. The path to a file to be processed.
#' @param design_factors_path \code{string}. The path to a file containing design factor values.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_process_table_type_3(data_file_path, design_factors_path)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_process_table_type_3 <- function(data_file_path, design_factors_path) {

  design <- readRDS(design_factors_path)

  tryCatch({
    df <- readRDS(data_file_path)
  }, error = function(cond) {
    message(paste0("Couldn't read file: ", data_file_path))})

  year <- paste0("20", sub(".*_ *(.*?) *.Rds*", "\\1", data_file_path))
  col_2_name <- names(df)[2]
  names(df)[2] <- "temp_variable_name"
  row_order <- unique(df$temp_variable_name)

  end_of_sequence <- length(colnames(df)) - length(colnames(df)[colnames(df) == "Base"]) - length(colnames(df)[colnames(df) == "All"])

  rename_columns <- colnames(df)[3:end_of_sequence]
  colnames <- names(df)

  df$Year <- year
  df <- df[, c("Year", colnames)]

  first_gather_column_index <- 4
  last_gather_column_index <- length(names(df)) - length(colnames(df)[colnames(df) == "Base"]) - length(colnames(df)[colnames(df) == "All"])

  df <- df %>%
    tidyr::gather(key = gather_key,
                  value = "Percent",
                  first_gather_column_index:last_gather_column_index) %>%
    dplyr::group_by(Year, Council, gather_key) %>%
    merge(design, by = "Year") %>%
    dplyr::mutate(sig_value = 1.96 * as.numeric(Factor) * (sqrt((as.numeric(Percent) / 100) * (1 - (as.numeric(Percent) / 100)) / as.numeric(Base))),
                  sig_lower = as.numeric(Percent) - (100 * sig_value),
                  sig_upper = as.numeric(Percent) + (100 * sig_value)
                  ) %>%
    dplyr::ungroup()

  colnames(df)[3] <- col_2_name

  percent_values <- df %>%
    dplyr::select("Year", "Council", col_2_name, "gather_key", "Percent", if ("All" %in% colnames(df)) {"All"}, "Base") %>%
    tidyr::spread("gather_key", "Percent")

  column_order <- c("Year", "Council", col_2_name, rename_columns, if ("All" %in% colnames(df)) {"All"}, "Base")
  percent_values <- percent_values[, column_order]

  sig_lower_string <- paste0("sig_lower_values <- df %>% dplyr::select(`Year`, `Council`, `",
                             col_2_name,
                             "`, `gather_key`, `sig_lower`) %>% tidyr::spread(key = `gather_key`, value = `sig_lower`) %>% dplyr::rename(")

  for (column_name in rename_columns) {

    sig_lower_string <- paste0(sig_lower_string, "`", column_name, "_l` = `", column_name, "`, ")
  }

  sig_lower_string <- (substr(sig_lower_string, 1, nchar(sig_lower_string) - 2)) %>%
    paste0(")")

  sig_upper_string <- paste0("sig_upper_values <- df %>% dplyr::select(`Year`, `Council`, `",
                             col_2_name,
                             "`, `gather_key`, `sig_upper`) %>% tidyr::spread(key = `gather_key`, value = `sig_upper`) %>% dplyr::rename(")

  for (column_name in rename_columns) {

    sig_upper_string <- paste0(sig_upper_string, "`", column_name, "_u` = `", column_name, "`, ")
  }

  sig_upper_string <- (substr(sig_upper_string, 1, nchar(sig_upper_string) - 2)) %>%
    paste0(")")

  get_order_string <- paste0("df$`", col_2_name, "`  <- factor(df$`", col_2_name, "`, levels = row_order)")

  reorder_string <- paste0("df <- df[order(df$`", col_2_name, "`),]")

  eval(parse(text = sig_lower_string))
  eval(parse(text = sig_upper_string))
  significance_length <- length(sig_lower_values)
  df <- dplyr::bind_cols(c(percent_values, sig_lower_values[4:significance_length], sig_upper_values[4:significance_length]))
  eval(parse(text = get_order_string))
  eval(parse(text = reorder_string))

  df
}
