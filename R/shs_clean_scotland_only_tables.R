#' Remove local authority data from Scotland-only tables
#'
#' \code{shs_clean_scotland_only_tables} removes local authority data from tables
#' where sample size is too small to show local authority data.
#'
#' @param app_dataset_directory \code{string}.
#' The path of the directory table data is stored in.
#' @param app_metadata_directory \code{string}.
#' The path of the directory metadata tables are stored in.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_clean_scotland_only_tables(app_dataset_directory, app_metadata_directory)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_clean_scotland_only_tables <- function(app_dataset_directory, app_metadata_directory) {

  question_titles <- readRDS(file.path(app_metadata_directory, "question_titles.Rds"))

  scotland_only_tables <- question_titles[!is.na(question_titles$ScotlandOnly) & question_titles$Type != 0,]
  scotland_only_tables <- scotland_only_tables[scotland_only_tables$ScotlandOnly == "Y",]$ID

  for (scotland_only_table in scotland_only_tables) {

    data_file_path <- file.path(app_dataset_directory, paste0(scotland_only_table, ".Rds"))

    df <- readRDS(data_file_path)

    df <- df[df$Council == "Scotland", ]

    file.remove(data_file_path)
    saveRDS(df, data_file_path)
  }
}
