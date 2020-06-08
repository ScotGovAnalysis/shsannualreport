# library(magrittr)

question_titles <- readRDS("~/shsannualreport/app/data/metadata/question_titles.Rds")

source("source/report_functions.R")$values
source("source/functions.R")$values
source("source/table_processing.R")$values


# question <- "Figure 3.3" # type 0 working
# question <- "Table 2.1" # type 1 working
question <- "Table 2.6" # type 2
# question <- "Table 2.10" # type 3
# question <- "Table 3.1b" # type 4

local_authority <- "Scotland"
year <- "2018"
comparison_type <- "Local Authority/Scotland"
# comparison_type <- "Local Authority"
comparator <- "Aberdeenshire"

table <- table_processing(question, local_authority, year, comparison_type, comparator)
table <- table[!grepl("Year", colnames(table)) & !grepl("Council", colnames(table))]

table

comparison_df <- table

comparison_columns <- colnames(comparison_df)[grep("_2", colnames(comparison_df))]

if (length(comparison_columns) > 0) {

  variable_column_name <- colnames(comparison_df)[1]

  significance_columns <- colnames(comparison_df)[grep("_sig", colnames(comparison_df))]

  comparison_df <- comparison_df[c(variable_column_name, comparison_columns, significance_columns)]

  colnames(comparison_df) <- gsub("_2", "", colnames(comparison_df))

} else {

  comparison_df <- NULL
}


table_df <- comparison_df[!grepl("_l", colnames(comparison_df)) & !grepl("_u", colnames(comparison_df))]

hide_columns <- grep("_sig", colnames(table_df))
start_of_hide <- hide_columns[1]
end_of_hide <- hide_columns[length(hide_columns)]
hide_columns <- paste0(start_of_hide, ":", end_of_hide)

variable_column_names <- colnames(table_df)[3:start_of_hide - 1]

main_df_comparison_output("table_df", variable_column_names, hide_columns)

eval(parse(text = main_df_comparison_output("table_df", variable_column_names, hide_columns)))

round_string(column_variables = column_variables, comparison_year_present = comparison_year_present)

if (length(colnames(table)[grep("_2", colnames(table))]) > 0) {
  table <- dplyr::mutate(table,
                         `All adults` = ifelse(`All adults` > 0, suppressWarnings(as.character(round(as.numeric(`All adults`,  0)))), `All adults`),
                         `Adults aged 16 to 64` = ifelse(`Adults aged 16 to 64` > 0, suppressWarnings(as.character(round(as.numeric(`Adults aged 16 to 64`,  0)))), `Adults aged 16 to 64`),
                         `All adults_2` = ifelse(`All adults_2` > 0, suppressWarnings(as.character(round(as.numeric(`All adults_2`,  0)))), `All adults_2`),
                         `Adults aged 16 to 64_2` = ifelse(`Adults aged 16 to 64_2` > 0, suppressWarnings(as.character(round(as.numeric(`Adults aged 16 to 64_2`,  0)))), `Adults aged 16 to 64_2`))
  } else {
    table <- dplyr::mutate(table,
                           `All adults` = ifelse(`All adults` > 0, suppressWarnings(as.character(round(as.numeric(`All adults`,  0)))), `All adults`),
                           `Adults aged 16 to 64` = ifelse(`Adults aged 16 to 64` > 0, suppressWarnings(as.character(round(as.numeric(`Adults aged 16 to 64`,  0)))), `Adults aged 16 to 64`))
  }
