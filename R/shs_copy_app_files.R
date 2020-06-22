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

  file.copy("inst/app.R", app_directory, overwrite = TRUE)
  file.copy("inst/home.Rmd", app_directory, overwrite = TRUE)
  file.copy("inst/.gitignore", app_directory, overwrite = TRUE)

  file.copy("inst/functions.R", app_source_directory, overwrite = TRUE)

  file.copy("inst/ci_graph.png", app_www_directory, overwrite = TRUE)
  file.copy("inst/home_logo.png", app_www_directory, overwrite = TRUE)
  file.copy("inst/modal-into.png", app_www_directory, overwrite = TRUE)
  file.copy("inst/modal_chart.png", app_www_directory, overwrite = TRUE)
  file.copy("inst/modal_download.png", app_www_directory, overwrite = TRUE)
  file.copy("inst/modal_survey2.png", app_www_directory, overwrite = TRUE)
  file.copy("inst/modal_table2.png", app_www_directory, overwrite = TRUE)
  file.copy("inst/nat_stat.png", app_www_directory, overwrite = TRUE)
  file.copy("inst/new_logo.png", app_www_directory, overwrite = TRUE)
  file.copy("inst/SG_master_logo_RGB.jpg", app_www_directory, overwrite = TRUE)
  file.copy("inst/shs-logo.png", app_www_directory, overwrite = TRUE)
  file.copy("inst/sign_table.png", app_www_directory, overwrite = TRUE)
  file.copy("inst/styles.css", app_www_directory, overwrite = TRUE)
}
