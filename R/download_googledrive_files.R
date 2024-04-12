#' Download Google Drive Files
#'
#' For a given Google Drive folder this function will find and download all files
#' matching a given pattern.
#'
#' Note: This relies on the `googledrive::drive_ls()` function which uses a search function
#' and is not deterministic when recursively searching.
#'
#' @param key_path character path to google authentication key
#' @param drive_path character The google drive folder path
#' @param MIME_type character Google Drive file type, file extension, or MIME type. 
#' @param out_path  character The local file directory for files to be downloaded to
#' @param search_pattern character A search pattern for files in the google drive
#'
#' @return vector a character vector of files downloaded
download_googledrive_files <-
  function(key_path = here::here("./auth/rvf2-workflow-automation-2877f538ddb9.json"),
           drive_path,
           search_pattern,
           MIME_type = NULL,
           out_path) {
    
    # Handle authentication
    googledrive::drive_auth(path = key_path)
    googlesheets4::gs4_auth(path = key_path)
    
    # recursive search of drive folder for a given file type
    files <- googledrive::drive_ls(
      path = as_id(drive_path),
      recursive = T,
      pattern = search_pattern,
      type = googledrive::drive_mime_type(MIME_type)
    )
    
    # automatically assign file type extension
    files_w_ext <- files |> 
      mutate(extension = map_chr(drive_resource, `[[`, "fileExtension"),
             path = paste0(id, ".", extension))
    
    # download all files
    map2(.x = files_w_ext$id, .y = files_w_ext$path, ~drive_download(.x, path = paste0(out_path, .y), overwrite = TRUE))

    # return list of downloaded files for tracking
    file_list <- list.files(path = out_path, pattern = search_pattern, full.names = T)
    
    # return list of downloaded files for tracking
    file_list <- list.files(path = out_path, pattern = search_pattern, full.names = T)

    return(file_list)
  }
