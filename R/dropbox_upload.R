#' Dropbox Upload
#'
#' Upload a local file to dropbox and handle authentication.
#'
#' @details
#' This is a wrapper of `rdrop2::drop_upload()` which first reads in a local
#' CSV file and then uploads to a DropBox path.
#'
#' @param log Validation Log for OH cleaning pipelines
#' @param file_path local file path for upload
#' @param dropbox_path relative dropbox path
#'
#' @return performs drop box upload
#' @export
#' @examples
#' \dontrun{
#'     dropbox_upload(
#'     kzn_animal_ship_semiclean,
#'     file_path = here::here("outputs/kzn_animal_ship_semiclean.csv"),
#'     dropbox_path = "DTRA_RVF2/Data/rvf2_github_data/semi_clean_data"
#'     )
#' }
#'
dropbox_upload <- function(log, file_path, dropbox_path) {
  log_export <- readr::write_csv(log, file_path)

  # upload token
  refresh_db_token()

  # upload
  rdrop2::drop_upload(file_path, dropbox_path)

}
