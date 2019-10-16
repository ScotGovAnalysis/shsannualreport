#' Process SHS table of type 4
#'
#' \code{shs_process_table_type_4} cleans and formats data contained in an .Rds file in order to make the
#' data suitable for use in the SHS Annual Report.
#'
#' @param data_file_path \code{string}. The path to a file to be processed.
#' @param design_factors_path \code{string}. The path to a file containing design factor values.
#' @param gather_key \code{string}. The name of the column to gather under.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_process_table_type_4(data_file_path, design_factors_path)
#'
#' @keywords internal
#'
#' @noRd

shs_process_table_type_4 <- function(data_file_path, design_factors_path, gather_key) {
  df <- readRDS(data_file_path)
  design <- readRDS(design_factors_path)
  year <- paste0("20", sub(".*_ *(.*?) *.Rds*", "\\1", data_file_path))
  col_2_name <- names(df)[2]
  names(df)[2] <- "temp_variable_name"

  colnames <- names(df)
  df$Year = year
  df <- df[, c("Year", colnames)]

  first_gather_column_index <- 4
  last_gather_column_index <- length(names(df)) - 1

  df <- tidyr::gather(df, key=gather_key, value=Percent, first_gather_column_index:last_gather_column_index)

    df$ReplaceNaPercent <- df$Percent

    df <- df %>% dplyr::mutate(ReplaceNaPercent = as.numeric(ReplaceNaPercent))

    df$ReplaceNaPercent[is.na(df$ReplaceNaPercent)] <- 0

    #df$Base[df$Base == "-"] <- 10

    df <- df %>% dplyr::group_by(Council, Year, gather_key)

    df <- merge(df, design, by = "Year")

    df <- dplyr::mutate(df, sig_value = 1.96 * Factor * (sqrt((ReplaceNaPercent / 100) * (1 - (ReplaceNaPercent / 100)) / Base)),
                  LowerConfidenceLimit = round(ReplaceNaPercent - (100 * sig_value), 2),
                  UpperConfidenceLimit = round(ReplaceNaPercent + (100 * sig_value), 2),
                  ReplaceNaPercent = round(ReplaceNaPercent, 1))

    df <- dplyr::ungroup(df)
    df <- dplyr::select(df, -8, -9)

  names(df)[3] <- col_2_name
  names(df)[5] <- gather_key

  df
}
