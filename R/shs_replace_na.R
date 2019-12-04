#' Replace NA values in processed dataframes
#'
#' \code{shs_replace_na} replaces NA values in processed dataframes with hyphens, or asterisks for incomplete years' data
#'
#' @param data_file_path \code{string}. A path to an Rds file to replace values in.
#' @param incomplete_years \code{character vector}. The path to a file containing design factor values.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_replace_na(dataframe, c(incomplete_year_1, incomplete_year_2))
#' }
#'
#' @export


shs_replace_na <- function(data_file_path, incomplete_years) {

  dataframe <- readRDS(data_file_path)

  if (length(incomplete_years) > 0) {

    for (incomplete_year in incomplete_years) {

      if (length(colnames(dataframe)[colnames(dataframe) == "2011"]) > 0) {

        replace_string <- paste0("dataframe[is.na(dataframe$`", incomplete_year, "`),]$`", incomplete_year, "` <- '*'")

        eval(parse(text = replace_string))
      }
    }
  }

  dataframe[is.na(dataframe)] <- "-"

  saveRDS(dataframe, data_file_path)
}
