#' Copy static files from package into app project
#'
#' \code{shs_copy_app_files} copies necessary static files from package into newly created app project.
#'
#' @param app_directory \code{string}.
#' The path of the directory the app has been created in.
#' @param app_source_directory \code{string}.
#' The path of the app directory containing source files (variables and functions).
#' @param app_www_directory \code{string}.
#' The path of the app directory containing stylesheets and images.
#'
#' @return \code{null}.
#'
#' @examples
#' \dontrun{
#' shs_copy_app_files(app_directory, app_source_directory, app_www_directory)
#' }
#'
#' @keywords internal
#'
#' @noRd

shs_copy_app_files <- function(app_directory, app_source_directory, app_www_directory) {

  package_path <- file.path(.libPaths()[1], "shsannualreport")

  file.copy(file.path(package_path, "app.R"), app_directory, overwrite = TRUE)
  file.copy(file.path(package_path, "home.Rmd"), app_directory, overwrite = TRUE)
  file.copy(file.path(package_path, ".gitignore"), app_directory, overwrite = TRUE)

  file.copy(file.path(package_path, "functions.R"), app_source_directory, overwrite = TRUE)

  file.copy(file.path(package_path, "ci_graph.png"), app_www_directory, overwrite = TRUE)
  file.copy(file.path(package_path, "home_logo.png"), app_www_directory, overwrite = TRUE)
  file.copy(file.path(package_path, "modal-into.png"), app_www_directory, overwrite = TRUE)
  file.copy(file.path(package_path, "modal_chart.png"), app_www_directory, overwrite = TRUE)
  file.copy(file.path(package_path, "modal_download.png"), app_www_directory, overwrite = TRUE)
  file.copy(file.path(package_path, "modal_survey2.png"), app_www_directory, overwrite = TRUE)
  file.copy(file.path(package_path, "modal_table2.png"), app_www_directory, overwrite = TRUE)
  file.copy(file.path(package_path, "nat_stat.png"), app_www_directory, overwrite = TRUE)
  file.copy(file.path(package_path, "new_logo.png"), app_www_directory, overwrite = TRUE)
  file.copy(file.path(package_path, "SG_master_logo_RGB.jpg"), app_www_directory, overwrite = TRUE)
  file.copy(file.path(package_path, "shs-logo.png"), app_www_directory, overwrite = TRUE)
  file.copy(file.path(package_path, "sign_table.png"), app_www_directory, overwrite = TRUE)
  file.copy(file.path(package_path, "styles.css"), app_www_directory, overwrite = TRUE)
}
