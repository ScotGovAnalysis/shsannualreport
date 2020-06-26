#' Create R variables to use in the Shiny App
#'
#' \code{shs_create_shiny_variables} creates a file of R variables to based on extracted data,
#' to be used in the SHS annual report Shiny app.
#'
#' @param reports_start_year \code{double}.
#' The first year of data available for selection in report builder.
#' @param reports_end_year \code{string}.
#' The final year of data available for selection in report builder.
#' @param app_source_directory \code{string}.
#' The path of the app directory containing source files (variables and functions).
#' @param app_dataset_directory \code{string}.
#' The path of the app directory containing the dataset.
#' @param app_metadata_directory \code{string}.
#' The path of the app directory containing metadata.
#'
#' @return \code{double}.
#'
#' @examples
#' \dontrun{
#' shs_create_shiny_variables(reports_start_year, reports_end_year, app_source_directory, app_dataset_directory, app_metadata_directory)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_create_shiny_variables <- function(reports_start_year,
                                       reports_end_year,
                                       app_source_directory,
                                       app_dataset_directory,
                                       app_metadata_directory) {

  save_file_path <- file.path(app_source_directory, "variables.R")

  file.create(save_file_path)

  topic_titles <- readRDS(file.path(app_metadata_directory, "topic_titles.Rds"))
  question_titles <- readRDS(file.path(app_metadata_directory, "question_titles.Rds"))

  question_titles$HasDataFile <- "N"

  for (ID in question_titles$ID) {

    if (paste0(ID, ".Rds") %in% list.files(app_dataset_directory)) {

      question_titles[question_titles$ID == ID, ]$HasDataFile <- "Y"

    }
  }

  question_titles <- question_titles[question_titles$HasDataFile == "Y" | question_titles$Type == 0, ]

  cat("question_titles <- readRDS(\"data/metadata/question_titles.Rds\")\n\n", file = save_file_path, append = TRUE)

  cat("topic_titles <- readRDS(\"data/metadata/topic_titles.Rds\")\n\n", file = save_file_path, append = TRUE)

  cat("time_series_colours <- brewer.pal(8, \"Dark2\")\n\n", file = save_file_path, append = TRUE)

  cat("shs_colours <- c(\"#01665e\", # Dark green
      \"#57a0d3\", # Darkish blue
      \"#81d8d0\", # Teal
      \"#7285a5\", # Grey
      \"#cc79a7\", # Pink
      \"#e69f00\", # Yellow
      \"#542788\", # Purple
      \"#66a61e\", # Green
      \"#d95f02\", # Burnt orange
      \"#a6761d\", # Yellow-Brown
      \"#666666\", # Dark grey
      \"#fb9a99\", # Peach
      \"#e31a1c\", # Red
      \"#fdbf6f\", # Yellow
      \"#ff7f00\", # Bright orange
      \"#cab2d6\", # Light purple
      \"#6a3d9a\", # Strong purple
      \"#ebd72a\", # Bright yellow
      \"#b15928\"  # Brown
  )\n\n", file = save_file_path, append = TRUE)

  cat("local_authorities <- c(\"Scotland\",
                           \"Aberdeen City\",
                           \"Aberdeenshire\",
                           \"Angus\",
                           \"Argyll & Bute\",
                           \"Clackmannanshire\",
                           \"Dumfries & Galloway\",
                           \"Dundee City\",
                           \"East Ayrshire\",
                           \"East Dunbartonshire\",
                           \"East Lothian\",
                           \"East Renfrewshire\",
                           \"Edinburgh, City of\",
                           \"Falkirk\",
                           \"Fife\",
                           \"Glasgow City\",
                           \"Highland\",
                           \"Inverclyde\",
                           \"Midlothian\",
                           \"Moray\",
                           \"Na h-Eileanan Siar\",
                           \"North Ayrshire\",
                           \"North Lanarkshire\",
                           \"Orkney Islands\",
                           \"Perth & Kinross\",
                           \"Renfrewshire\",
                           \"Scottish Borders\",
                           \"Shetland Islands\",
                           \"South Ayrshire\",
                           \"South Lanarkshire\",
                           \"Stirling\",
                           \"West Dunbartonshire\",
                           \"West Lothian\")\n\n",
      file = save_file_path,
      append = TRUE)

  years_string <- "years <- c("

  year <- reports_end_year

  while (year >= reports_start_year) {

    years_string <- paste0(years_string, "\"", year, "\", ")

    year <- year - 1
  }

  years_string <- (substr(years_string, 1, nchar(years_string) - 2))

  years_string <- paste0(years_string, ")\n\n")

  cat(years_string,
      file = save_file_path,
      append = TRUE)

  select_topic_string <- "select_list_topics <- c("

  counter <- 2
  for (topic_title in topic_titles[topic_titles$has_data == "y", ]$title) {

    select_topic_string <- paste0(select_topic_string, "\"Topic ", counter, ": ",
                                  topic_title, "\" = \"", topic_title, "\",\n")
    counter <- counter + 1
  }

  select_topic_string <- (substr(select_topic_string, 1, nchar(select_topic_string) - 2)) %>%
    paste0(")\n\n")

  cat(select_topic_string, file = save_file_path, append = TRUE)

  topic_numbers <- gsub("Top", "", topic_titles$code)

  for (topic_number in topic_numbers) {

    select_question_string <- ""

    question_ids_by_topic <- question_titles[grepl(paste0(" ", topic_number, "."), question_titles$ID), ]$ID

    if (length(question_ids_by_topic) > 0) {

      select_question_string <- paste0(select_question_string, "select_list_questions_topic_", topic_number, " <- c(")

      for (question_id in question_ids_by_topic) {

        question_title <- question_titles[question_titles$ID == question_id, ]$Title

        select_question_string <- paste0(select_question_string, "\"", question_id, ": ",
                                         question_title, "\"", " = ", "\"", question_id, "\",", "\n")
      }

      if (length(question_ids_by_topic) > 0) {

        select_question_string <- (substr(select_question_string, 1, nchar(select_question_string) - 2))
      }

      select_question_string <- paste0(select_question_string, ")\n\n")

      cat(select_question_string, file = save_file_path, append = TRUE)
    }
  }

  cat("type_0_questions <- question_titles$ID[question_titles$Type == 0]

type_1_questions <- question_titles$ID[question_titles$Type == 1]
type_2_questions <- question_titles$ID[question_titles$Type == 2]
type_3_questions <- question_titles$ID[question_titles$Type == 3]
type_4_questions <- question_titles$ID[question_titles$Type == 4]\n",
      file = save_file_path,
      append = TRUE)
}
