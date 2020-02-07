#' Process SHS annual report data for publication
#'
#' \code{shs_process_data} processes raw SHS survey data which has been extracted from Excel sheets into individual .Rds files.
#' It relies on a number of internal functions: \code{shs_process_table_type_1} (for time series tables),
#' \code{shs_process_table_type_2} (for column percentage tables), \code{shs_process_table_type_3} (for row percentage tables),
#' \code{shs_process_table_type_4} (for manually added tables).
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_process_data()
#' }
#'
#' @export

shs_process_data <- function() {

  extracted_dataset_path <- "app\\data\\dataset"
  extracted_metadata_path <- "app\\data\\metadata"

  question_titles <- readRDS(file.path(extracted_metadata_path, "question_titles.Rds"))
  data_files <- list.files(extracted_dataset_path)
  design_factors_path <- file.path(extracted_metadata_path, "design_factors.Rds")

  type_0_tables <- dplyr::filter(question_titles, Type == 0)$ID
  type_1_tables <- dplyr::filter(question_titles, Type == 1)$ID
  type_2_tables <- dplyr::filter(question_titles, Type == 2)$ID
  type_3_tables <- dplyr::filter(question_titles, Type == 3)$ID
  type_4_tables <- dplyr::filter(question_titles, Type == 4)$ID

  # Data processing for type 1 [time series] datasets

  missing_tables_type_1 <- c()

  for (table in type_1_tables) {

    save_file_path <- file.path(extracted_dataset_path, paste0(table, ".Rds"))

    if (grepl(", ", table)) {

      data_file_path <- NULL

      multiple_tables <- strsplit(table, split = ", ")

      for (single_table in multiple_tables[[1]]) {

        if (length(data_files[grepl(paste0(toupper(single_table), ".RDS"), toupper(data_files))]) == 1) {

          data_file_path <- file.path(extracted_dataset_path, data_files[grepl(paste0(toupper(single_table), ".RDS"), toupper(data_files))])

        } else if (length(data_files[grepl(paste0(toupper(single_table), "_LA.RDS"), toupper(data_files))]) == 1) {

          data_file_path <- file.path(extracted_dataset_path, data_files[grepl(paste0(toupper(single_table), "_LA.RDS"), toupper(data_files))])
        }
      }

      if (is.null(data_file_path)) {

        missing_tables_type_1 <- c(missing_tables_type_1, table)
      }

    } else if (length(data_files[grepl(paste0(toupper(table), "_LA.RDS"), toupper(data_files))]) == 1) {

      data_file_path <- file.path(extracted_dataset_path, data_files[grepl(paste0(toupper(table), "_LA.RDS"), toupper(data_files))])

    } else if (length(data_files[grepl(paste0(toupper(table), ".RDS"), toupper(data_files))]) == 1) {

      data_file_path <- file.path(extracted_dataset_path, data_files[grepl(paste0(toupper(table), ".RDS"), toupper(data_files))])

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

  if (length(missing_tables_type_1) > 0) {

    print(paste0("Type 1 files not found: ", missing_tables_type_1))
  }

  # Data processing for type 2 (column percentage) datasets

  missing_tables_type_2 <- c()

  for (table in type_2_tables) {

    save_file_path <- file.path(extracted_dataset_path, paste0(table, ".Rds"))

    if (grepl(", ", table)) {

      data_file_path <- NULL

      multiple_tables <- strsplit(table, split = ", ")

      for (single_table in multiple_tables[[1]]) {

        if (length(data_files[grepl(paste0(toupper(single_table), "_"), toupper(data_files))]) == 1) {

          files <- data_files[grepl(paste0(toupper(single_table), "_"), toupper(data_files))]
        }
      }

    } else {

      files <- data_files[grepl(toupper(paste0(table, "_")), toupper(data_files))]
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
            final_df <- plyr::rbind.fill(final_df, df)

          }, error = function(cond) {

            message(paste0("rbind for file: ", file, " threw error: ", cond))
          })
        }

        file.remove(data_file_path)
        saveRDS(final_df, save_file_path)

      }
    }
  }

  if (length(missing_tables_type_2) > 0) {

    print(paste0("Type 2 files not found: ", missing_tables_type_2))
  }

  # Data processing for type 3 (row percentage) datasets

  missing_tables_type_3 <- c()

  for (table in type_3_tables) {

    save_file_path <- file.path(extracted_dataset_path, paste0(table, ".Rds"))

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

            final_df <- plyr::rbind.fill(final_df, df)

          }, error = function(cond) {

            message(paste0("rbind for file: ", file, " threw error: ", cond))
          })
        }

        file.remove(data_file_path)
        saveRDS(final_df, save_file_path)
      }
    }
  }

  if (length(missing_tables_type_3) > 0) {

    print(paste0("Type 3 files not found: ", missing_tables_type_3))
  }

  # Data processing for type 4 (manual input) datasets

  missing_tables_type_4 <- c()

  for (table in type_4_tables) {

    save_file_path <- file.path(extracted_dataset_path, paste0(table, ".Rds"))

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

  if (length(missing_tables_type_4) > 0) {

    print(paste0("Type 4 files not found: ", missing_tables_type_4))
  }
}
