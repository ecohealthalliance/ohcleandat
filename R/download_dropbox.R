#' Download Drop Box Files
#'
#' Downloads files from dropbox into a given directory
#'
#' @param dropbox_path character The formal folder path on dropbox
#' @param dropbox_filename character The formal file name on dropbox
#' @param download_path character Local file path to download file to
#' @param ... other arguments passed to rdrop2::drop_download
#'
#' @return returns file path if successful
#' @export
#' @seealso [rdrop2::drop_download()]
#' @examples
#' \dontrun{
#'    download_dropbox(dropbox_path = "DTRA_RVF2/RVF Mosquito Datasets",
#'    dropbox_filename = "Mosquito dataset as at 01-02-2024.xlsx",
#'    download_path = here::here("data"),
#'    overwrite = TRUE)
#' }
#'
download_dropbox <-
  function(dropbox_path,
           dropbox_filename,
           download_path,
           ...) {
    # locate and update token
    refresh_db_token()

    # check if exists
    if (!rdrop2::drop_exists(paste(dropbox_path, dropbox_filename, sep = "/"))) {
      stop("The dropbox file could not be found. Please check the path and token permissions.")
    }

    file <-
      rdrop2::drop_download(
        path = paste(dropbox_path, dropbox_filename, sep = "/"),
        local_path = download_path,
        ...
      )

    if (file == TRUE) {
      out <- paste(download_path, dropbox_filename, sep = "/")
    } else {
      out <- file
    }

    return(out)
  }
