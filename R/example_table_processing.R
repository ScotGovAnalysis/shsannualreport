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
comparison_type <- "No comparison"
# comparison_type <- "Local Authority"
comparator <- NULL

table <- table_processing(question, local_authority, year, comparison_type, comparator)
table <- table[colnames(table) != "Year" & !grepl("Council", colnames(table))]

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

DT::datatable(table_df, colnames = gsub("blank", "", colnames(table_df)), options = list(digits = 1, na = '-', paging = FALSE, ordering = FALSE, info = FALSE, searching = FALSE, columnDefs = list(list(targets = c(0, 4:5), visible = FALSE)))) %>% formatStyle(c('All adults','Adults aged 16 to 64'), c('All adults_sig','Adults aged 16 to 64_sig'), backgroundColor = styleEqual(c('NO', 'HIGHER', 'LOWER'),c('transparent', '#00A3A3', '#C3C3FF')))




