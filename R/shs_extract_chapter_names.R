#' Extract chapter titles from Excel
#'
#' \code{shs_extract_chapter_titles} extracts data from Excel workbooks in a specified location,
#' and saves all sheets as .Rds files in a newly created location.
#'
#' @return \code{null}.
#'
#' @examples
#' shs_extract_chapter_titles()
#'
#' @export

shs_extract_chapter_titles <- function() {
  title_directory <- file.path("titles", "titles.xls")
  print(title_directory)
}
