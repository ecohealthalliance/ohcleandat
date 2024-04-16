#' Download Google Drive Files
#'
#' For a given Google Drive folder this function will find and download all files
#' matching a given pattern.
#'
#' @details Note: This relies on the `googledrive::drive_ls()` function which uses a search function
#' and is not deterministic when recursively searching. Please pay attention to what is returned.
#'
#' @param key_path character path to Google authentication key
#' @param drive_path character The Google drive folder path
#' @param MIME_type character Google Drive file type, file extension, or MIME type.
#' @param out_path  character The local file directory for files to be downloaded to
#' @param search_pattern character A search pattern for files in the Google drive
#'
#' @return a character vector of files downloaded
#' @export
#' @seealso [googledrive::drive_ls()]
#' @examples
#' \dontrun{
#'   download_googledrive_files(
#'   key_path = here::here("./key.json"),
#'   drive_path = "https://drive.google.com/drive/u/0/folders/asdjfnasiffas8ef7y7y89rf",
#'   search_pattern = "*.xlsx",
#'   out_path = here::here("data/project_data/")
#'   )
#' }
#'
download_googledrive_files <-
  function(key_path,
           drive_path,
           search_pattern,
           MIME_type = NULL,
           out_path) {
    # Handle authentication
    googledrive::drive_auth(path = key_path)
    googlesheets4::gs4_auth(path = key_path)

    # recursive search of drive folder for a given file type
    files <- googledrive::drive_ls(
      path = googledrive::as_id(drive_path),
      recursive = T,
      pattern = search_pattern,
      type = googledrive::drive_mime_type(MIME_type)
    )

    # automatically assign file type extension
    files_w_ext <- files |>
      dplyr::mutate(
        extension = purrr::map_chr(drive_resource, `[[`, "fileExtension"),
        path = paste0(id, ".", extension)
      )

    # download all files
    purrr::map2(
      .x = files_w_ext$id,
      .y = files_w_ext$path,
      ~ googledrive::drive_download(.x, path = paste0(out_path, .y), overwrite = TRUE)
    )

    # return list of downloaded files for tracking
    file_list <-
      list.files(path = out_path,
                 pattern = search_pattern,
                 full.names = T)

    # return list of downloaded files for tracking
    file_list <-
      list.files(path = out_path,
                 pattern = search_pattern,
                 full.names = T)

    return(file_list)
  }
