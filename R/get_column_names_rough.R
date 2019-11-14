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
#Get variable values
# df <- readRDS("extracted 14-Nov-2019 11.54.40\\dataset\\Table 4.3_LA_18.Rds")
# colnames(df)
#
# df[,2]
# unique(df[,2])

extracted_data_path <- "extracted 14-Nov-2019 14.49.41"
extracted_dataset_path <- file.path(extracted_data_path, "dataset")

files <- list.files(extracted_dataset_path)

all_variable_names <- c()

for (file in files) {
# ###
# file <- "Table 9.7_LA.Rds"
# ###
    file_path <- file.path(extracted_dataset_path, file)
    df <- readRDS(file_path)

    if ("sort" %in% colnames(df)){
      df <- subset(df, select=-c(sort))
    }

    if ("_LABEL_" %in% colnames(df)){
      df <- subset(df, select=-c(`_LABEL_`))
    }

    variable_names <- unique(df[2])[[1]]
    all_variable_names <- paste(all_variable_names, variable_names)
}

all_variable_names <- unique(all_variable_names)

all_variable_names <- data.frame(all_variable_names)

