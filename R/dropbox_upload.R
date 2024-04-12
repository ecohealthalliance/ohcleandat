#' Dropbox Upload  
#' 
#' Upload a local file to dropbox and handle authentication
#'
#' @param file path to local file for upload
#' @param path relative dropbox path
#'
#' @return performs drop box upload
#' @export
#'
dropbox_upload <- function(log, file_path, dropbox_path){
  
  log_export <- readr::write_csv(log, file_path)
  
  # upload token
  refresh_db_token()
  
  # upload
  rdrop2::drop_upload(file_path, dropbox_path)

}