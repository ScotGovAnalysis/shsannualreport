#' Add or update the 'source_data_directory' in .Renviron
#'
#' \code{shs_set_source_data_directory} adds or updates the 'source_data_directory' key in .Renviron file
#'
#' \code{shs_set_source_data_directory} adds or updates the 'source_data_directory' key in \code{.Renviron} using the internal function \code{shs_set_renviron_key}.
#' The \code{directory_path} parameter must be passed an existing directory path, which is where source data is stored.
#' Backslashes must be escaped, e.g. \code{C:\\Documents\\directoryName} must be input as \code{C:\\\\Documents\\\\directoryName}.
#'
#' @param directory_path \code{string}. An existing directory path, which is where source data is stored.
#'
#' @return \code{logical}.
#' \code{TRUE} when key is successfully updated.
#' If \code{FALSE} will return error message.
#'
#' @export

shs_set_source_data_directory <- function(directory_path) {
  shs_set_renviron_key("source_data_directory", directory_path)
}
