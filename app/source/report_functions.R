# main_table_string ####
main_table_string <- function(question_type, year_present) {

  print("main_table_string")

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

  print("comparison_table_string")

  if (question_type %in% c("1", "4")) {

    comparison_table_string <- "table_comparison <- table[table$Council == comparator,] %>% dplyr::rename(`Council_2` = `Council`, "

  } else if(question_type %in% c("2", "3")) {

    if (comparison_type == "Local Authority") {

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

  print("merge_string")

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

  print("remove_significance_string")

  paste0("table[table$`", row_variable, "` == 'All' | table$`", row_variable, "` == 'Base', colnames(table)[grep('_sig', colnames(table))]] <- \"NO\"")
}

# arrange_select_mutate_string ####
arrange_select_mutate_string <- function(comparison_type, question_type, row_variable, column_variables, year_present, comparison_year_present) {

  print("arrange_select_mutate_string")

  if (question_type == "0" | year_present == FALSE) {

    arrange_select_mutate_string <- "table <- NULL"

  } else if (question_type %in% c("1", "2", "3") & year_present == TRUE) {

    arrange_select_mutate_string <- paste0("table <-  dplyr::arrange(table, `", row_variable, "`) %>% ",
                                           "dplyr::select(`", row_variable, "`, ")

    for (column_variable in column_variables) {

      addition_string <- paste0("`", column_variable, "`, ")

      arrange_select_mutate_string <- paste0(arrange_select_mutate_string, addition_string)
    }

    if ((question_type %in% c("2", "3") & comparison_type != "No comparison" & comparison_year_present == TRUE) | (question_type == "1" & !comparison_type %in% c("Year", "No comparison"))) {

      for (column_variable in column_variables) {

        addition_string <- paste0("`", column_variable, "_2`, ")

        arrange_select_mutate_string <- paste0(arrange_select_mutate_string, addition_string)
      }

      for (column_variable in column_variables) {

        if (!column_variable %in% c("All", "Base")) {

          addition_string <- paste0("`", column_variable, "_sig`, ")

          arrange_select_mutate_string <- paste0(arrange_select_mutate_string, addition_string)

        }
      }

      arrange_select_mutate_string <- substr(arrange_select_mutate_string, 1, nchar(arrange_select_mutate_string) - 2)

      arrange_select_mutate_string <- paste0(arrange_select_mutate_string, ") %>% dplyr::mutate(")

      for (column_variable in column_variables) {

        addition_string <- paste0("`", column_variable, "` = ifelse(`", column_variable, "` > 0, suppressWarnings(as.character(round(as.numeric(`", column_variable, "`,  0)))), `", column_variable, "`), ")

        arrange_select_mutate_string <- paste0(arrange_select_mutate_string, addition_string)
      }

      for (column_variable in column_variables) {

        addition_string <- paste0("`", column_variable, "_2` = ifelse(`", column_variable, "_2` > 0, suppressWarnings(as.character(round(as.numeric(`", column_variable, "_2`,  0)))), `", column_variable, "_2`), ")

        arrange_select_mutate_string <- paste0(arrange_select_mutate_string, addition_string)
      }
    }

    arrange_select_mutate_string <- paste0(substr(arrange_select_mutate_string, 1, nchar(arrange_select_mutate_string) - 2), ")")

  } else if (question_type == "4") {

    arrange_select_mutate_string <- "table <- table[, -which(names(table) %in% c(\"Council\", \"Council_2\"))]"
  }

  arrange_select_mutate_string
}

