#' Add or update .Renviron key
#'
#' \code{shs_set_renviron_key} adds or updates a key in .Renviron file
#'
#' \code{shs_set_renviron_key} adds or updates a key in \code{.Renviron}.
#' It is an internal function, called in \code{shs_set_source_data_folder},
#' preventing other keys being accidentally altered.
#' If there is no existing .Renviron in the home directory it is created.
#' For changes to take effect the R session must be restarted.
#' The \code{renviron_key} parameter must be passed a name for the key.
#' The \code{value} parameter must be passed the value to assign to the key.
#'
#' @param key \code{string}. The key to add or update.
#' @param value \code{string}. The value to apply to the key.
#'
#' @return \code{logical}.
#' \code{TRUE} when key is successfully updated.
#' If \code{FALSE} will return error message.
#'
#' @keywords internal
#'
#' @noRd

shs_set_renviron_key <- function(key, value) {
  renviron <- file.path(Sys.getenv("R_USER"), ".Renviron")

  if (file.exists(renviron) == FALSE) {
    file.create(renviron)
  }

  current_lines <- readLines(renviron)
  con <- file(renviron, open = "r")
  lines_to_keep <- c()

  if (length(current_lines) != 0) {
    while (TRUE) {
      line <- readLines(con, n = 1)
      if (length(line) == 0) break
      else if (!startsWith(line, key)){
        lines_to_keep <- c(lines_to_keep, line)
      }
    }
  }
  writeLines(paste(key, ' = "', value, '"', sep = ""), renviron)
  write(lines_to_keep, file = renviron, append = TRUE)
  close.connection(con)
}
