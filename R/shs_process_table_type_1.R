#' Process SHS table of type 1
#'
#' \code{shs_process_table_type_1} cleans and formats data contained in an .Rds file in order to make the
#' data suitable for use in the SHS Annual Report.
#'
#' @param data_file_path \code{string}. The path to a file to be processed.
#' @param design_factor_path \code{string}. The path to a file containing design factor values.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_process_table_type_1(data_file_path, design_factor_path)
#'
#' @keywords internal
#'
#' @noRd

shs_process_table_type_1 <- function(data_file_path, design_factor_path) {

# Read in files from parameters
df <- readRDS(data_file_path)
design <- readRDS(design_factor_path)

# Rename names in spreadsheet (capitilise)

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

# Get min and max years
min_year <- year_columns[which.min(year_columns)][[1]]
max_year <- year_columns[which.max(year_columns)][[1]]


table <- tidyr::gather(df, key=Year, value=Percent, min_year:max_year) %>%
  dplyr::mutate(Percent = as.numeric(Percent)) %>%
  dplyr::group_by(Council, Year) %>%
  dplyr::mutate(n = Percent[Ethnicity== "Base"]) %>%
  merge(design, by = "Year") %>%
  dplyr::mutate(sig_value = 1.96 * Factor * (sqrt((Percent / 100) * (1 - (Percent / 100)) / n)),
                LowerConfidenceLimit = round(Percent - (100 * sig_value), 2),
                UpperConfidenceLimit = round(Percent + (100 * sig_value), 2),
                Percent = round(Percent, 1)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-6, -7, -8)

saveRDS(table, data_file_path)
}
# mutate(n = as.numeric(table[table[, 2] == "Base",]$Percent))
