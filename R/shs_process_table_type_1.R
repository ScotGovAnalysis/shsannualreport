#' Process SHS table of type 1
#'
#' \code{shs_process_table_type_1} cleans and formats data contained in an .Rds file in order to make the
#' data suitable for use in the SHS Annual Report.
#'
#' @param data_file_path \code{string}. The path to a file to be processed.
#' @param design_factors_path \code{string}. The path to a file containing design factor values.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_process_table_type_1(data_file_path, design_factors_path)
#'
#' @keywords internal
#'
#' @noRd

shs_process_table_type_1 <- function(data_file_path, design_factors_path) {

# Read in files from parameters
df <- readRDS(data_file_path)
design <- readRDS(design_factors_path)

# Create Lists of column types
year_columns <- list()

# Loop through column names
for (name in names(df)) {
 if (grepl("^[0-9]{1,}$", name)) {
   # Assign numeric columns to year value list
   year_columns <- c(year_columns, name)
   } else {
     # Get the name of the non-year column that is not "Council" or "All"
     if (name != "Council" & name != "All")
     non_year_column <- name
   }
}

names(df)[2] <- "temp_variable_name"

# Get min and max years
min_year <- year_columns[which.min(year_columns)][[1]]
max_year <- year_columns[which.max(year_columns)][[1]]


df <- tidyr::gather(df, key=Year, value=Percent, min_year:max_year) %>%
  dplyr::mutate(Percent = as.numeric(Percent)) %>%
  dplyr::group_by(Council, Year) %>%
  dplyr::mutate(n = Percent[temp_variable_name == "Base"]) %>%
  merge(design, by = "Year") %>%
  dplyr::mutate(sig_value = 1.96 * Factor * (sqrt((Percent / 100) * (1 - (Percent / 100)) / n)),
                LowerConfidenceLimit = round(Percent - (100 * sig_value), 2),
                UpperConfidenceLimit = round(Percent + (100 * sig_value), 2),
                Percent = round(Percent, 1)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-6, -7, -8)

names(df)[3] <- non_year_column

saveRDS(df, data_file_path)
}
