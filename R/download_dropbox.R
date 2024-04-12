#' Download Drop Box
#'
#' Downloads files from dropbox into a given directory
#'
#' @param dropbox_path character The formal folder path on dropbox
#' @param dropbox_filename character The formal file name on dropbox
#' @param download_path character Local file path to download file to
#' @param ... other arguments passed to rdrop2::drop_download
#'
#' @return returns file path if successful

download_dropbox <- function(dropbox_path, dropbox_filename, download_path, ...) {
  
  # locate and update token
  refresh_db_token()
  
  # check if exists
  if (!rdrop2::drop_exists(paste(dropbox_path, dropbox_filename, sep = "/"))) {
    stop("The dropbox file could not be found. Please check the path and token permissions.")
  }
  
  file <- rdrop2::drop_download(path = paste(dropbox_path, dropbox_filename, sep = "/"),
                        local_path = download_path,
                        ...)
  
  if(file == TRUE){
    out <- paste(download_path, dropbox_filename, sep = "/")
  } else {
    out <- file
  }
  
  return(out)
  
}