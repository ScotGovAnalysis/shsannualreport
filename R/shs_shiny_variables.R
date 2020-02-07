#' Create R variables to use in the Shiny App
#'
#' \code{shs_shiny_variables} creates a file of R variables to based on extracted data, to be used in the SHS annual report Shiny app.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_shiny_variables()
#' }
#'
#' @export

shs_shiny_variables <- function() {

  save_file_path <- "app\\source\\variables.R"

  file.create(save_file_path)

  extracted_dataset_path <- "app\\data\\dataset"
  extracted_metadata_path <- "app\\data\\metadata"

  chapter_titles <- readRDS(file.path(extracted_metadata_path, "chapter_titles.Rds"))
  question_titles <- readRDS(file.path(extracted_metadata_path, "question_titles.Rds"))

  files <- list.files(extracted_dataset_path)

  cat("question_titles <- readRDS(\"data\\\\metadata\\\\question_titles.Rds\")\n\n", file = save_file_path, append = TRUE)

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

  select_chapter_string <- "select_list_chapters <- c("

  counter = 2
  for (chapter_title in chapter_titles[chapter_titles$has_data == 'y',]$title) {

    select_chapter_string <- paste0(select_chapter_string, "\"Ch. ", counter, ": ", chapter_title, "\" = \"", chapter_title, "\",\n")
    counter = counter + 1
  }

  select_chapter_string <- (substr(select_chapter_string, 1, nchar(select_chapter_string) - 2)) %>%
    paste0(")\n\n")

  cat(select_chapter_string, file = save_file_path, append = TRUE)

  chapter_numbers <- gsub("CH", "", chapter_titles$code)

  for (chapter_number in chapter_numbers) {

    select_question_string <- ""
    valid_question_ids <- c()
    question_ids_by_chapter <- question_titles[grepl(paste0(" ", chapter_number, "."), question_titles$ID),]$ID

    if (length(question_ids_by_chapter) > 0) {

      select_question_string <- paste0(select_question_string, "select_list_questions_chapter_", chapter_number, " <- c(")

      for (question_id in question_ids_by_chapter) {

        question_title <- question_titles[question_titles$ID == question_id,]$Title

        select_question_string <- paste0(select_question_string, "\"", question_id, ": ", question_title, "\"", " = ", "\"", question_id, "\",", "\n")
      }

      if (length(question_ids_by_chapter) > 0) {

        select_question_string <- (substr(select_question_string, 1, nchar(select_question_string) - 2))
      }

      select_question_string <- paste0(select_question_string, ")\n\n")

      cat(select_question_string, file = save_file_path, append = TRUE)
    }
  }

  cat("type_0_questions <- question_titles$ID[question_titles$Type == 0]\n\ntype_1_questions <- question_titles$ID[question_titles$Type == 1]\n\ntype_2_questions <- question_titles$ID[question_titles$Type == 2]\n\ntype_3_questions <- question_titles$ID[question_titles$Type == 3]\n\ntype_4_questions <- question_titles$ID[question_titles$Type == 4]\n\n",
      file = save_file_path,
      append = TRUE)
}

