extracted_data_path <- "extracted 30-Oct-2019 12.49.12"

extracted_dataset_path <- file.path(extracted_data_path, "dataset")
extracted_metadata_path <- file.path(extracted_data_path, "metadata")

# shsannualreport:::shs_process_column_names(extracted_dataset_path, extracted_metadata_path)

question_titles <- readRDS(file.path(extracted_metadata_path, "question_titles.Rds"))
data_files <- list.files(extracted_dataset_path)
design_factors_path <- file.path(extracted_metadata_path, "design_factors.Rds")

# print("Processing type 1 datasets")
# for (table in dplyr::filter(question_titles, Type == 1)$ID) {
#   question_title <- gsub("/", " ", dplyr::filter(question_titles, ID == table)$Title)
#   save_file_path <- file.path(extracted_dataset_path, paste0(question_title, ".Rds"))
#
#   if (length(data_files[grep(paste0(table, "_LA.Rds"), data_files)]) == 1) {
#     data_file_path <- file.path(extracted_dataset_path, data_files[grep(paste0(table, "_LA.Rds"), data_files)])
#   } else if (length(data_files[grep(paste0(table, ".Rds"), data_files)]) == 1) {
#     data_file_path <- file.path(extracted_dataset_path, data_files[grep(paste0(table, ".Rds"), data_files)])
#   } else {
#     data_file_path <- "test"
#     }
#   if (data_file_path == "extracted 30-Oct-2019 11.21.37/dataset/Figure 5.4_LA.Rds") {
#   shsannualreport:::shs_process_table_type_1(data_file_path, design_factors_path, save_file_path)
#   } else {print("test")}
# }

# "Table 11.10" "Table 11.18" "Figure 12.2" "Figure 12.7"
# for (table in dplyr::filter(question_titles, Type == 2)$ID) {
# table <- "Figure 12.7"
#   question_title <- gsub("/", " ", dplyr::filter(question_titles, ID == table)$Title)
#   save_file_path <- file.path(extracted_dataset_path, paste0(question_title, ".Rds"))
#   files <- data_files[grepl(toupper(table), toupper(data_files))]
#   final_df <- readRDS(file.path(extracted_dataset_path, files[1]))
#   names(final_df)[3] <- "Percent"
#   final_df$"Year" <- NA
#   final_df <- final_df[0,]
#   for (file in files) {
#     data_file_path <- file.path(extracted_dataset_path, file)
#     # print(data_file_path)
#     df <- shsannualreport:::shs_process_table_type_2(data_file_path, design_factors_path)
#     final_df <- rbind(final_df, df)
#     file.remove(data_file_path)
#   }
#   saveRDS(final_df, save_file_path)
# }

# for (table in dplyr::filter(question_titles, Type == 3)$ID) {
  # "Table 2.6_13.Rds" "Table 2.6_14.Rds" "Table 2.6_15.Rds" "Table 2.6_16.Rds" "Table 2.6_17.Rds" "Table 2.6_18.Rds"
table <- "Table 2.6"
  question_title <- gsub("/", " ", dplyr::filter(question_titles, ID == table)$Title)
  save_file_path <- file.path(extracted_dataset_path, paste0(question_title, ".Rds"))
  files <- data_files[grepl(toupper(table), toupper(data_files))]
  print(files)

for (file in files) {
  data_file_path <- file.path(extracted_dataset_path, file)
  print(data_file_path)
  print(design_factors_path)
  # df <- shsannualreport:::shs_process_table_type_3(data_file_path, design_factors_path)
  # file.remove(data_file_path)
}
# saveRDS(final_df, save_file_path)
}

# shs_process_table_type_3 <- function(data_file_path, design_factors_path, gather_key) {
   df <- readRDS(data_file_path)
  design <- readRDS(design_factors_path)
year <- paste0("20", sub(".*_ *(.*?) *.Rds*", "\\1", data_file_path))
col_2_name <- names(df)[2]
names(df)[2] <- "temp_variable_name"
#
  colnames <- names(df)
  df$Year = year
  df <- df[, c("Year", colnames)]
#
  first_gather_column_index <- 4
  last_gather_column_index <- length(names(df))
#
  df <- tidyr::gather(df, key=gather_key, value=Percent, first_gather_column_index:last_gather_column_index) %>%
    dplyr::mutate(Percent = as.numeric(Percent)) %>%
    dplyr::group_by(Council, Year, gather_key) %>%
    dplyr::mutate(n = Percent[temp_variable_name == "Base"]) %>%
    merge(design, by = "Year") %>%
    dplyr::mutate(sig_value = 1.96 * Factor * (sqrt((Percent / 100) * (1 - (Percent / 100)) / n)),
                  LowerConfidenceLimit = round(Percent - (100 * sig_value), 2),
                  UpperConfidenceLimit = round(Percent + (100 * sig_value), 2),
                  Percent = round(Percent, 1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-6, -7, -8)

  names(df)[3] <- col_2_name

  values <- df %>%
    select("Council", "Economic status","gather_key", "Percent") %>%
    spread(key = "gather_key", value = "Percent" )

  sig_lower <- df %>%
    select("Council", "Economic status","gather_key", "LowerConfidenceLimit") %>%
    spread(key = "gather_key", value = "LowerConfidenceLimit" ) %>%
    rename(  "Council_l" = "Council",
              "Economic status_l" = "Economic status",
              "All adults_l"=  "All adults",
              "Working age adults_l" =  "Working age adults")

  sig_upper <- df %>%
    select("Council", "Economic status","gather_key", "UpperConfidenceLimit") %>%
    spread(key = "gather_key", value = "UpperConfidenceLimit" ) %>%
    rename(  "Council_u" = "Council",
             "Economic status_u" = "Economic status",
             "All adults_u"=  "All adults",
             "Working age adults_u" =  "Working age adults")


  #Combine the 3 datasets together to have full dataset with all significance values
  all <- bind_cols(c(values, sig_lower, sig_upper)) %>%
    select(-Council_l, -Council_u, -`Economic status_l`, -`Economic status_u`)

  paste0(question_title, " [", year, "]")

#   df
# }
