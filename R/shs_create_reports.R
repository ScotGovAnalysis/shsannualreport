#' Create Rmd reports for each chapter
#'
#' \code{shs_create_reports} creates an Rmd report file for each topic in the SHS annual report data.
#'
#' @param app_metadata_directory \code{string}.
#' The path of the app directory containing metadata.
#' @param app_reports_directory \code{string}.
#' The path of the app directory to save report files to.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_create_reports(app_metadata_directory, app_reports_directory)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_create_reports <- function(app_metadata_directory, app_reports_directory) {

  topics <- readRDS(file.path(app_metadata_directory, "topic_titles.Rds"))
  topics <- topics[topics$has_data == "y",]

  questions <- readRDS(file.path(app_metadata_directory, "question_titles.Rds"))

  for (row in 1:nrow(topics)) {

    topic_id <- topics[row, "code"]
    title <- topics[row, "title"]
    report_file_path <- file.path(app_reports_directory, paste0(topic_id, ".Rmd"))

    number <- sub("Top", "", topic_id)
    topic_questions <- questions[questions$Topic == number,]

    if (file.exists(report_file_path)) {

      unlink(report_file_path)
    }

    file.create(report_file_path)

    connection <- file(report_file_path, "wt")

    string <- "---
params:
  report_title: \"\"
  author: \"\"
  date: \"\"
  local_authority: \"\"
  year: \"\"
  topic_data: \"\"
  comparison_type: \"\"
  comparator: \"\"
title: \"`r params$report_title`\"
author: \"`r params$author`\"
date: \"`r params$date`\"
output:
  pdf_document:
    toc: yes
    fig_caption: false
header-includes:
    - \\hypersetup{colorlinks=true, linkcolor = black, urlcolor = [RGB]{0, 163, 163}}
    - \\usepackage{titling}
    - \\pretitle{\\begin{center}
      \\includegraphics[width=2in,height=2in]{../www/new_logo.png}\\LARGE\\\\}
    - \\posttitle{\\end{center}}
    - \\renewcommand{\\familydefault}{\\sfdefault}
classoption: landscape
fontsize: 10pt
papersize: a4
geometry: margin=1.5cm
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=FALSE)
options(tinytex.verbose = TRUE)

library(knitr)
library(dplyr)
library(kableExtra)
library(png)
library(tidyselect)

local_authority <- params$local_authority
year <- params$year
topic_data <- params$topic_data
comparison_type <- params$comparison_type
comparator <- params$comparator

"
    writeLines(iconv(string, to = "UTF-8"), connection, useBytes=T)

    counter <- 1

    for (row in 1:nrow(topic_questions)) {

      question_id <- topic_questions[row, "ID"]
      question_id_underscore <- gsub(",", "", question_id)
      question_id_underscore <- gsub(" ", "_", question_id_underscore)

      string <- paste0(question_id_underscore, " <- topic_data[[", counter, "]]
")

      writeLines(iconv(string, to = "UTF-8"), connection, useBytes=T)

      counter <- counter + 1

    }

    string <- "
main_table_title <- paste0(local_authority, \", \", year)

eval_comparison <- ifelse(comparison_type == \"No comparison\", FALSE, TRUE)

eval_comparison_time_series <- ifelse(comparison_type == \"Year\" | comparison_type == \"No comparison\", FALSE, TRUE)

comparison_table_title <- if (comparison_type == \"Local Authority/Scotland\") {
  paste0(comparator, \", \", year)
} else if (comparison_type == \"Year\") {
  paste0(local_authority, \", \", comparator)
} else {
  NULL
}

if (comparison_type == \"Local Authority/Scotland\") {

main_significance_key <- asis_output(paste0(\"```{=latex}
  $ \\\\color[RGB]{0, 163, 163} \\\\blacksquare $ Significantly greater than \", comparator, \" (\", year, \")  $ \\\\color[RGB]{195, 195, 255} \\\\blacksquare $ Significantly lower than \", comparator, \" (\", year, \")
  ```\"))

comparison_significance_key <- asis_output(paste0(\"```{=latex}
  $ \\\\color[RGB]{0, 163, 163} \\\\blacksquare $ Significantly greater than \", local_authority, \" (\", year, \")  $ \\\\color[RGB]{195, 195, 255} \\\\blacksquare $ Significantly lower than \", local_authority, \" (\", year, \")
  ```\"))

main_significance_key_time_series <- asis_output(paste0(\"```{=latex}
  $ \\\\color[RGB]{0, 163, 163} \\\\blacksquare $ Significantly greater than \", comparator, \"  $ \\\\color[RGB]{195, 195, 255} \\\\blacksquare $ Significantly lower than \", comparator, \"
  ```\"))

comparison_significance_key_time_series <- asis_output(paste0(\"```{=latex}
  $ \\\\color[RGB]{0, 163, 163} \\\\blacksquare $ Significantly greater than \", local_authority, \"  $ \\\\color[RGB]{195, 195, 255} \\\\blacksquare $ Significantly lower than \", local_authority, \"
  ```\"))


} else if (comparison_type == \"Year\") {

  main_significance_key <- asis_output(paste0(\"```{=latex}
  $ \\\\color[RGB]{0, 163, 163} \\\\blacksquare $ Significantly greater than \", local_authority, \" (\", comparator, \")  $ \\\\color[RGB]{195, 195, 255} \\\\blacksquare $ Significantly lower than \", local_authority, \" (\", comparator, \")
  ```\"))

  comparison_significance_key <- asis_output(paste0(\"```{=latex}
  $ \\\\color[RGB]{0, 163, 163} \\\\blacksquare $ Significantly greater than \", local_authority, \" (\", year, \")  $ \\\\color[RGB]{195, 195, 255} \\\\blacksquare $ Significantly lower than \", local_authority, \" (\", year, \")
  ```\"))

} else {

  main_significance_key <- \"\"

  comparison_significance_key <- \"\"
}

```

## Acknowledgements
The Scottish Government acknowledges and thanks the 10,530
people across Scotland who gave their time to take part in the
Scottish Household Survey 2018.
This report was produced by the Scottish Household Survey Project Team
at the Scottish Government.
We would also like to thank all the Scottish Government lead analysts who
contributed to the project.
Finally, special thanks to Ipsos MORI and their interviewers for continuous
and relentless efforts during the fieldwork.

\\pagebreak
"
    writeLines(iconv(string, to = "UTF-8"), connection, useBytes=T)

    counter_2 <- 1

    for (row in 1:nrow(topic_questions)) {

      question_id <- topic_questions[row, "ID"]
      question_id_underscore <- gsub(",", "", question_id)
      question_id_underscore <- gsub(" ", "_", question_id_underscore)
      title <- topic_questions[row, "Title"]
      type <- topic_questions[row, "Type"]
      coverage <- topic_questions[row, "Coverage"]
      comment <- topic_questions[row, "Comment"]
      link <- topic_questions[row, "Link"]

      if (type != "0") {

        data_file_path <- paste0("app/data/dataset/", question_id, ".Rds")

        column_names <- colnames(readRDS(data_file_path))
        main_column_names <- column_names[!grepl("_l", column_names) & !grepl("_u", column_names)]

        significance_column_names <- gsub("_l", "", column_names[grep("_l", column_names)])
        significance_column_names <- significance_column_names[!significance_column_names %in% c("All", "Base")]

        max_variable_length <- 0

        if (type %in% c("1", "4")) {

          max_variable_length <- max(nchar(as.character(readRDS(data_file_path)[[2]])))

        } else if (type %in% c("2", "3")) {

          max_variable_length <- max(nchar(as.character(readRDS(data_file_path)[[3]])))

        }

        if (type == 1) {

          markdown_comparator <- "eval_comparison_time_series"
          main_key <- "main_significance_key_time_series"
          comparison_key <- "comparison_significance_key_time_series"

        } else {

          markdown_comparator <- "eval_comparison"
          main_key <- "main_significance_key"
          comparison_key <- "comparison_significance_key"
        }

      }

      string <- paste0("## ", question_id, ": ", title, "\n")

      if (type == 0) {

        string <- paste0(string, "### Base numbers at local authority level are too small to produce robust analysis.\n")

      } else if (type %in% c(1, 2)) {

        string <- paste0(string, "### Column percentages, ", coverage, "\n")

      } else if (type == 3) {

        string <- paste0(string, "### Row percentages, ", coverage, "\n")

      } else if (type == 4) {

        string <- paste0(string, "### Grossed-up estimates (Rounded to the nearest 10,000)\n")
      }

      if (!is.na(comment)) {

        string <- paste0(string, "### ", comment, "\n")
      }

      if (!is.na(link)) {

        link_comment_pattern = ">(.*?)<"
        link_comment <- regmatches(link, regexec(link_comment_pattern, link))[[1]][2]

        url_pattern = "<a href=\"(.*?)\">"
        url <- regmatches(link, regexec(url_pattern, link))[[1]][2]

        string <- paste0(string, "### [", link_comment, "]", "(", url, ")\n")
      }

      if (type == "0") {
        string <- paste0(string, "
\\pagebreak
")
      }

      if (type == "1") {
        string <- paste0(string, "
### `r local_authority`
")
      }

      if (type %in% c("2", "3")) {
        string <- paste0(string, "
### `r main_table_title`
")
      }

      if (type %in% c("1", "2", "3")) {
        string <- paste0(string, "
```{r eval=(", markdown_comparator, " == FALSE)}\n",
                         "if (!is.null(", question_id_underscore, ")) {\n",
                         question_id_underscore, " %>%
kable(\"latex\", col.names = gsub(\"blank\", \"\", colnames(", question_id_underscore, ")), escape = TRUE, booktabs = T)")

        if ((length(main_column_names) > 12) | (Reduce("+", nchar(main_column_names)) > 150)) {

          string <- paste0(string, " %>% kable_styling(latex_options = \"scale_down\")")
        }

        if (max_variable_length > 50) {

          string <- paste0(string, " %>% column_spec(1, width = \"20em\")")
        }

        string <- paste0(string, "\n } else {
        asis_output(\"### There is no data to show for this table within the specified parameters\")
      }
```

```{r eval=", markdown_comparator, "}
if (!is.null(", question_id_underscore, ")) {
if (length(grep(\"_2\", colnames(", question_id_underscore, "))) > 0) {
colnames(", question_id_underscore, ") <- gsub(\"%\", \"\\\\\\\\%\", colnames(", question_id_underscore, "))
main_column_names <- colnames(", question_id_underscore, ")[!grepl(\"_2\", colnames(", question_id_underscore, "))]
significance_column_names <- colnames(", question_id_underscore, ")[grepl(\"_sig\", colnames(", question_id_underscore, "))]
presentation_column_names <-  main_column_names[!main_column_names %in% significance_column_names]

", question_id_underscore, " %>% select(tidyselect::all_of(main_column_names)) %>%
mutate(")

        for (significance_column_name in significance_column_names) {

          significance_column_name <- gsub("%", "\\\\\\\\%", significance_column_name)

          append_string <- paste0("`", significance_column_name, "` = cell_spec(`", significance_column_name,
                                  "`, \"latex\", background = case_when(`", significance_column_name,
                                  "_sig` == \"HIGHER\" ~ \"#00A3A3\", `", significance_column_name, "_sig` == \"LOWER\" ~ \"#C3C3FF\", TRUE ~ \"#FFFFFF\")),\n")

          string <- paste0(string, append_string)

        }

        string <- paste0((substr(string, 1, nchar(string ) - 2)),
                         "\n", "
) %>%
select(!tidyselect::all_of(significance_column_names)) %>%
kable(\"latex\", col.names = gsub(\"blank\", \"\", presentation_column_names), escape = FALSE, booktabs = T)")

        if (max_variable_length > 50) {

          string <- paste0(string, " %>% column_spec(1, width = \"20em\")")
        }

        if ((length(main_column_names) > 12) | (Reduce("+", nchar(main_column_names)) > 150)) {

          string <- paste0(string, " %>% kable_styling(latex_options = \"scale_down\")")
        }

        string <- paste0(string, "
} else {
", question_id_underscore, " %>%
kable(\"latex\", col.names = gsub(\"blank\", \"\", colnames(", question_id_underscore, ")), escape = TRUE, booktabs = T)")

        if (max_variable_length > 50) {

          string <- paste0(string, " %>% column_spec(1, width = \"20em\")")
        }

        if ((length(main_column_names) > 12) | (Reduce("+", nchar(main_column_names)) > 150)) {

          string <- paste0(string, " %>% kable_styling(latex_options = \"scale_down\")")
        }
        string <- paste0(string, "
}
} else {
  asis_output(\"### There is no data to show for this table within the specified parameters.\")
}
```
```{r eval=", markdown_comparator, "}
", main_key, "
```

```{r eval=", markdown_comparator, "}
")
        if (type  == "1") {
          string <- paste0(string, "
asis_output(paste0(\"### \", comparator))
  ")
        }

        if (type %in% c("2", "3")) {
          string <- paste0(string, "
asis_output(paste0(\"### \", comparison_table_title))
  ")
        }

        string <- paste0(string, "
```

```{r eval=", markdown_comparator, "}
if (!is.null(", question_id_underscore, ") && length(grep(\"_2\", colnames(", question_id_underscore, "))) > 0) {
comparison_column_names <- colnames(", question_id_underscore, ")[grepl(\"_2\", colnames(", question_id_underscore, "))]
comparison_rename_column_names <- gsub(\"_2\", \"\", comparison_column_names)

", question_id_underscore, " %>% select(tidyselect::all_of(colnames(", question_id_underscore, ")[!colnames(", question_id_underscore, ") %in% comparison_rename_column_names])) %>%
rename_at(comparison_column_names, ~ comparison_rename_column_names) %>%
mutate("
        )

        for (significance_column_name in significance_column_names) {

          significance_column_name <- gsub("%", "\\\\\\\\%", significance_column_name)

          append_string <- paste0("`", significance_column_name, "` = cell_spec(`", significance_column_name,
                                  "`, \"latex\", background = case_when(`", significance_column_name,
                                  "_sig` == \"HIGHER\" ~ \"#C3C3FF\", `", significance_column_name,
                                  "_sig` == \"LOWER\" ~ \"#00A3A3\", TRUE ~ \"#FFFFFF\")),\n")

          string <- paste0(string, append_string)

        }

        string <- paste0(substr(string, 1, nchar(string ) - 2),
                         ") %>%
        select(!tidyselect::all_of(significance_column_names)) %>%
        kable(\"latex\", col.names = gsub(\"blank\", \"\", presentation_column_names), escape = FALSE, booktabs = T)")

        if (max_variable_length > 50) {

          string <- paste0(string, " %>% column_spec(1, width = \"20em\")")
        }

        if ((length(main_column_names) > 12) | (Reduce("+", nchar(main_column_names)) > 150)) {

          string <- paste0(string, " %>% kable_styling(latex_options = \"scale_down\")")
        }

        string <- paste0(string, "\n} else {
asis_output(\"### There is no data to show for this table within the specified parameters, or there is no data to compare with.\")
}
```
```{r eval=", markdown_comparator, "}
", comparison_key, "
```
\\pagebreak
")
      }

if (type == 4) {
  string <- paste0(string, "
### `r main_table_title`

```{r eval=(eval_comparison_time_series == FALSE)}
", question_id_underscore, " %>%
kable(\"latex\", escape = FALSE, booktabs = T)")

  if (max_variable_length > 50) {

    string <- paste0(string, " %>% column_spec(1, width = \"20em\")")
  }

  if ((length(main_column_names) > 12) | (Reduce("+", nchar(main_column_names)) > 150)) {

    string <- paste0(string, " %>% kable_styling(latex_options = \"scale_down\")")
  }

  string <- paste0(string, "
```
```{r eval=eval_comparison_time_series}
main_column_names <- colnames(", question_id_underscore, ")[!grepl(\"_2\", colnames(", question_id_underscore, "))]

", question_id_underscore, " %>% select(tidyselect::all_of(main_column_names)) %>%
kable(\"latex\", escape = FALSE, booktabs = T)")

  if (max_variable_length > 50) {

    string <- paste0(string, " %>% column_spec(1, width = \"20em\")")
  }

  if ((length(main_column_names) > 12) | (Reduce("+", nchar(main_column_names)) > 150)) {

    string <- paste0(string, " %>% kable_styling(latex_options = \"scale_down\")")
  }

  string <- paste0(string, "
```
```{r eval=eval_comparison_time_series}
asis_output(paste0(\"### \", comparison_table_title))
```

```{r eval=eval_comparison_time_series}
comparison_column_names <- colnames(", question_id_underscore, ")[grepl(\"_2\", colnames(", question_id_underscore, "))]
comparison_rename_column_names <- gsub(\"_2\", \"\", comparison_column_names)

", question_id_underscore, " %>% select(tidyselect::all_of(colnames(", question_id_underscore, ")[!colnames(", question_id_underscore, ") %in% comparison_rename_column_names])) %>%
rename_at(comparison_column_names, ~ comparison_rename_column_names) %>%
kable(\"latex\", escape = FALSE, booktabs = T)")

  if (max_variable_length > 50) {

    string <- paste0(string, " %>% column_spec(1, width = \"20em\")")
  }

  if ((length(main_column_names) > 12) | (Reduce("+", nchar(main_column_names)) > 150)) {

    string <- paste0(string, " %>% kable_styling(latex_options = \"scale_down\")")
  }

  string <- paste0(string, "
```
\\pagebreak
")
}

writeLines(iconv(string, to = "UTF-8"), connection, useBytes=T)



counter_2 <- counter_2 + 1
    }
close(connection)
  }
}
