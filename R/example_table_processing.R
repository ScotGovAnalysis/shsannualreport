# library(magrittr)
question <- "Figure 3.3" # type 0
question <- "Table 2.1" # type 1
question <- "Table 2.6" # type 2
question <- "Table 2.10" # type 3
question <- "Table 3.1b" # type 4

local_authority <- "Scotland"
year <- "2001"
comparison_type <- "Local Authority"
comparator <- "Aberdeenshire"

table <- table_processing(question, local_authority, year, comparison_type, comparator)
table <- table[colnames(table) != "Year" & !grepl("Council", colnames(table))]

table

