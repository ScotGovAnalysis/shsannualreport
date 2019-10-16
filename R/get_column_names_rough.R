# extracted_dataset_path <- file.path("extracted 16-Oct-2019 11.05.19", "dataset")
#
# files <- list.files(extracted_dataset_path)
#
# all_column_names <- c()
#
# for (file in files) {
#   file_path <- file.path(extracted_dataset_path, file)
#   column_names <- colnames(readRDS(file_path))
#   all_column_names <- c(all_column_names, column_names)
# }
#
# all_column_names <- unique(all_column_names)
#
# all_column_names <- data.frame(all_column_names)
#
# View(all_column_names)
#
# write.csv(all_column_names, file = "my_data.csv")
#
#
