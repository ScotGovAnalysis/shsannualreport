#' Process SHS annual report data for publication
#'
#' \code{shs_process_data} processes raw SHS survey data which has been extracted from Excel sheets into individual .Rds files.
#' It relies on a number of internal functions: \code{shs_process_column_names}, TODO: update as more functions added.
#'
#' @param extracted_data_path \code{string}. The path of the directory data has previously been extracted to, by \code{shs_extract_data}.
#'
#' @return \code{null}.
#'
#' @examples
#'
#' \dontrun{
#' shs_process_data("extracted_data_path")
#' }
#'
#' @export

shs_process_data <- function(extracted_data_path) {

  # Set source directories
  extracted_dataset_path <- file.path(extracted_data_path, "dataset")
  extracted_metadata_path <- file.path(extracted_data_path, "metadata")

  # Process column_names
  # shsannualreport:::shs_process_column_names(extracted_dataset_path, extracted_metadata_path)

  question_titles <- readRDS(file.path(extracted_metadata_path, "question_titles.Rds"))
  data_files <- list.files(extracted_dataset_path)
  design_factors_path <- file.path(extracted_metadata_path, "design_factors.Rds")

  type_0_tables <- dplyr::filter(question_titles, Type == 0)$ID
  type_1_tables <- dplyr::filter(question_titles, Type == 1)$ID
  type_2_tables <- dplyr::filter(question_titles, Type == 2)$ID
  type_3_tables <- dplyr::filter(question_titles, Type == 3)$ID
  type_4_tables <- dplyr::filter(question_titles, Type == 4)$ID

  missing_tables_type_1 <- c()

  # Data processing for type 1 [time series] datasets
  # E.g. [variable]	_2013	_2014	_2015	_2016	_2017
  for (table in type_1_tables) {

    question_title <- gsub("/", " ", dplyr::filter(question_titles, ID == table)$Title)
    save_file_path <- file.path(extracted_dataset_path, paste0(question_title, ".Rds"))

    if (length(data_files[grep(paste0(table, "_LA.Rds"), data_files)]) == 1) {
      data_file_path <- file.path(extracted_dataset_path, data_files[grep(paste0(table, "_LA.Rds"), data_files)])
    } else if (length(data_files[grep(paste0(table, ".Rds"), data_files)]) == 1) {
      data_file_path <- file.path(extracted_dataset_path, data_files[grep(paste0(table, ".Rds"), data_files)])
    } else {
      data_file_path <- NULL
      missing_tables_type_1 <- c(missing_tables_type_1, table)
    }

    if (!is.null(data_file_path)) {
      tryCatch({
        shsannualreport:::shs_process_table_type_1(data_file_path, save_file_path, design_factors_path)
      }, error = function(cond) {
        message(paste0("Error processing type 1 table: ", table, " Error msg: ", cond))
      })
    }
  }

  print(paste0("Type 1 files not found: ", missing_tables_type_1))

  missing_tables_type_2 <- c()

  # Data processing for type 2 datasets
  # E.g [variable]	_2013	_All (with separate file for each year's data)
  for (table in type_2_tables) {

    print(table)

    question_title <- gsub("/", " ", dplyr::filter(question_titles, ID == table)$Title)
    save_file_path <- file.path(extracted_dataset_path, paste0(question_title, ".Rds"))
    if (grepl(" and ", table)) {
      both_tables <- strsplit(table, split = " and ")
      table_1 <- both_tables[[1]][1]
      table_2 <- both_tables[[1]][2]
      table_1_files <- data_files[grepl(toupper(table_1), toupper(data_files))]
      table_2_files <- data_files[grepl(toupper(table_2), toupper(data_files))]
      if (length(table_1_files > 0)) {
        files <- table_1_files
      } else if (length(table_2_files > 0)) {
        files <- table_2_files
      }
    } else {
      files <- data_files[grepl(toupper(table), toupper(data_files))]
    }

    if (length(files) == 0) {
      missing_tables_type_2 <- c(missing_tables_type_2, table)
    } else {

      for (file in files) {

        data_file_path <- file.path(extracted_dataset_path, file)

        tryCatch({
          df <- shsannualreport:::shs_process_table_type_2(data_file_path, design_factors_path)
        }, error = function(cond) {
          message(paste0("Error processing type 2 table: ", table, " Error msg: ", cond))
        })

        if (match(file, files) == 1) {

          final_df <- df

        } else {

          tryCatch({
            final_df <- rbind(final_df, df)
          }, error = function(cond) {
            message(paste0("rbind for file: ", file, " threw error: ", cond))
          })

        }

        # file.remove(data_file_path)
      }
    }

    saveRDS(final_df, save_file_path)
  }
  print(paste0("Type 2 files not found: ", missing_tables_type_2))

  missing_tables_type_3 <- c()

  # Data processing for type 3 datasets
  # E.g [variable]	_2013	_All (with separate file for each year's data)
  for (table in type_3_tables) {

    question_title <- gsub("/", " ", dplyr::filter(question_titles, ID == table)$Title)
    save_file_path <- file.path(extracted_dataset_path, paste0(question_title, ".Rds"))
    files <- data_files[grepl(toupper(table), toupper(data_files))]

    if (length(files) == 0) {
      missing_tables_type_3 <- c(missing_tables_type_3, table)
    } else {

      for (file in files) {

        data_file_path <- file.path(extracted_dataset_path, file)

        tryCatch({
          df <- shsannualreport:::shs_process_table_type_3(data_file_path, design_factors_path)
        }, error = function(cond) {
          message(paste0("Error processing type 3 table: ", table, " Error msg: ", cond))
        })

        if (match(file, files) == 1) {

          final_df <- df

        } else {

          tryCatch({
            final_df <- rbind(final_df, df)
          }, error = function(cond) {
            message(paste0("rbind for file: ", file, " threw error: ", cond))
          })

        }

        # file.remove(data_file_path)
      }
    }

    saveRDS(final_df, save_file_path)
  }

  print(paste0("Type 3 files not found: ", missing_tables_type_3))

  missing_tables_type_4 <- c()

  for (table in type_4_tables) {

    question_title <- gsub("/", " ", dplyr::filter(question_titles, ID == table)$Title)
    save_file_path <- file.path(extracted_dataset_path, paste0(question_title, ".Rds"))
    files <- data_files[grepl(toupper(table), toupper(data_files))]

    if (length(files) == 0) {
      missing_tables_type_4 <- c(missing_tables_type_4, table)
    } else {
      for (file in files) {

        data_file_path <- file.path(extracted_dataset_path, file)

        tryCatch({
          df <- shsannualreport:::shs_process_table_type_4(data_file_path, save_file_path)
        }, error = function(cond) {
          message(paste0("Error processing type 4 table: ", table, " Error msg: ", cond))
        })
      }
    }
  }

  print(paste0("Type 4 files not found: ", missing_tables_type_4))
}



