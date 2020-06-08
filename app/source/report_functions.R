# main_table_string ####
main_table_string <- function(question_type, year_present) {

  if (question_type == "0" | year_present == FALSE) {

    "table_main <- NULL"

  } else if (question_type %in% c("1", "4")) {

    "table_main <- table[table$Council == local_authority,]"

  } else if(question_type %in% c("2", "3") & year_present == TRUE) {

    "table_main <- table[table$Council == local_authority & table$Year == year,]"
  }
}

# comparison_table_string ####
comparison_table_string <- function(comparison_type, question_type, column_variables) {

  if (question_type %in% c("1", "4")) {

    comparison_table_string <- "table_comparison <- table[table$Council == comparator,] %>% dplyr::rename(`Council_2` = `Council`, "

  } else if(question_type %in% c("2", "3")) {

    if (comparison_type == "Local Authority/Scotland") {

      comparison_table_string <- "table_comparison <- table[table$Council == comparator & table$Year == year,] %>% dplyr::rename(`Council_2` = `Council`, "

    } else if (comparison_type == "Year") {

      comparison_table_string <- "table_comparison <- table[table$Council == local_authority & table$Year == comparator,] %>% dplyr::rename(`Year_2` = `Year`, "
    }
  }

  for (column_variable in column_variables) {

    addition_string <- paste0("`", column_variable, "_2` = `", column_variable, "`, ")

    comparison_table_string <- paste0(comparison_table_string, addition_string)
  }

  if (question_type %in% c("1", "2", "3")) {

    for (column_variable in column_variables) {

      if (!column_variable %in% c("All", "Base")) {

        addition_string <- paste0("`", column_variable, "_l_2` = `", column_variable, "_l`, ")

        comparison_table_string <- paste0(comparison_table_string, addition_string)
      }
    }

    for (column_variable in column_variables) {

      if (!column_variable %in% c("All", "Base")) {

        addition_string <- paste0("`", column_variable, "_u_2` = `", column_variable, "_u`, ")

        comparison_table_string <- paste0(comparison_table_string, addition_string)
      }
    }
  }

  comparison_table_string <- paste0(substr(comparison_table_string, 1, nchar(comparison_table_string) - 2), ")")
}

# merge_string ####
merge_string <- function(question_type, merge_by, row_variable, column_variables) {

  if (question_type %in% c("1", "2", "3")) {

    merge_string <- paste0("table <- merge(table_main, table_comparison, by = ", merge_by, ") %>%
    dplyr::mutate(")

    for (column_variable in column_variables) {

      if (!column_variable %in% c("All", "Base")) {

        addition_string <- paste0("`", column_variable, "_sig`= dplyr::case_when(`", column_variable, "_l` > `", column_variable, "_u_2` ~ 'HIGHER', `", column_variable, "_u` < `", column_variable, "_l_2` ~ 'LOWER', TRUE ~ 'NO'), ")

        merge_string <- paste0(merge_string, addition_string)

      }
    }

    merge_string <- paste0(substr(merge_string, 1, nchar(merge_string) - 2), ")")

  } else if (question_type == "4") {

    merge_string <- paste0("table <- plyr::join(table_main, table_comparison, by = ", merge_by, ")")
  }

  merge_string
}

# remove_significance_string ####
remove_significance_string <- function(row_variable) {

  paste0("table[table$`", row_variable, "` == 'All' | table$`", row_variable, "` == 'Base', colnames(table)[grep('_sig', colnames(table))]] <- \"NO\"")
}

# arrange_row_variables_string ####
arrange_row_variables_string <- function(row_variable) {

  paste0("table <-  dplyr::arrange(table, `", row_variable, "`)")
  }


# round_string ####
round_string <- function(table_name, column_variables) {

      round_string <- paste0("if (length(colnames(", table_name, ")[grep(\"_2\", colnames(", table_name, "))]) > 0) {
      ", table_name, " <- dplyr::mutate(", table_name, ", ")

      for (column_variable in column_variables) {

        addition_string <- paste0("`", column_variable, "` = ifelse(`", column_variable, "` > 0, suppressWarnings(as.character(round(as.numeric(`", column_variable, "`,  0)))), `", column_variable, "`), ")

        round_string <- paste0(round_string, addition_string)
      }

      for (column_variable in column_variables) {

        addition_string <- paste0("`", column_variable, "_2` = ifelse(`", column_variable, "_2` > 0, suppressWarnings(as.character(round(as.numeric(`", column_variable, "_2`,  0)))), `", column_variable, "_2`), ")

        round_string <- paste0(round_string, addition_string)
      }

    round_string <- paste0(substr(round_string, 1, nchar(round_string) - 2), ")
                           ",
                           "} else {
                           ", table_name, " <- dplyr::mutate(", table_name, ", ")

                           for (column_variable in column_variables) {

                             addition_string <- paste0("`", column_variable, "` = ifelse(`", column_variable, "` > 0, suppressWarnings(as.character(round(as.numeric(`", column_variable, "`,  0)))), `", column_variable, "`), ")

                             round_string <- paste0(round_string, addition_string)

                           }
    round_string <- paste0(substr(round_string, 1, nchar(round_string) - 2), ")
                           }")

    round_string
}

