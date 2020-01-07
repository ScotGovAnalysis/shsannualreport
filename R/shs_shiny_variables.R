#' Create R variables to use in the Shiny App
#'
#' \code{shs_shiny_variables} creates a file of R variables to based on extracted data, to be used in the SHS annual report Shiny app.
#'
#' @param save_file_path \code{string}. The path to save the file to.
#' @param design_factors_path \code{string}. The path of the extracted SHS data.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_shiny_variables(save_file_path, extracted_data_path)
#' }
#'
#' @export

shs_shiny_variables <- function(save_file_path, extracted_data_path) {

  file.create(save_file_path)

  extracted_dataset_path <- paste0(extracted_data_path, "\\dataset")
  extracted_metadata_path <- paste0(extracted_data_path, "\\metadata")

  chapter_titles <- readRDS(file.path(extracted_metadata_path, "chapter_titles.Rds"))
  question_titles <- readRDS(file.path(extracted_metadata_path, "question_titles.Rds"))

  type_0_tables <- dplyr::filter(question_titles, Type == 0)$ID
  type_1_tables <- dplyr::filter(question_titles, Type == 1)$ID
  type_2_tables <- dplyr::filter(question_titles, Type == 2)$ID
  type_3_tables <- dplyr::filter(question_titles, Type == 3)$ID
  type_4_tables <- dplyr::filter(question_titles, Type == 4)$ID

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
  print(chapter_numbers)

  for (chapter_number in chapter_numbers) {

    select_question_string <- ""
    valid_question_titles <- c()
    question_titles_by_chapter <- question_titles[grepl(paste0(" ", chapter_number, "."), question_titles$ID),]$Title

    if (length(question_titles_by_chapter) > 0) {

      select_question_string <- paste0(select_question_string, "select_list_questions_chapter_", chapter_number, " <- c(")

      for (question_title in question_titles_by_chapter){

        if (!is.na(pmatch(question_title, files)) || question_titles[question_titles$Title == question_title,]$ID %in% type_0_tables)

          valid_question_titles <- c(valid_question_titles, question_title)
      }
    }

    for (question_title in valid_question_titles) {

      question_title_without_special_character <- gsub("[[:punct:]]", "", question_title)

      question_number <- question_titles[grepl(question_title_without_special_character, question_titles$TitleWithoutSpecialCharacter),]$ID

      select_question_string <- paste0(select_question_string, "\"", question_number, ": ", question_title, "\"", " = ", "\"", question_title, "\",", "\n")
    }

    if (length(valid_question_titles) > 0) {

      select_question_string <- (substr(select_question_string, 1, nchar(select_question_string) - 2))
    }

    select_question_string <- paste0(select_question_string, ")\n\n")

    cat(select_question_string, file = save_file_path, append = TRUE)
  }

  question_types <- sort(unique(question_titles$Type))

  for (question_type in question_types){

    question_titles_by_type <- question_titles[question_titles$Type == question_type,]$Title
    questions_by_type_string <- paste0("type_", question_type, "_questions <- c(")

    for (question_title in question_titles_by_type) {

      questions_by_type_string <- paste0(questions_by_type_string, "\"", question_title, "\",\n")
    }

    if (length(question_titles_by_type) > 0) {

      questions_by_type_string <- (substr(questions_by_type_string, 1, nchar(questions_by_type_string) - 2))
      questions_by_type_string <- paste0(questions_by_type_string, ")\n\n")
    }

    cat(questions_by_type_string, file = save_file_path, append = TRUE)
  }
}

