#' Get Dropbox validation logs  
#' 
#' Downloads validation logs that are stored on dropbox
#' 
#' Ths function will check if the log exists and return NULL if not. Else it will
#' locally download the file to 'dropbox_validations' directory and read in to the
#' session. 
#'
#' @param file_name character file name with extension of the validation log
#' @param folder character the folder the log is saved in on drop box
#' @param path_name character the default drop box path
#'
#' @return tibble  Validation Log
#' 
get_dropbox_val_logs <- function(file_name, folder, path_name = "DTRA_RVF2/Data/rvf2_github_data/validation_logs") {
  
  refresh_db_token()
  
  # path handling when log isnt in a dedicated subfolder on drop box
  if (is.null(folder)) {
    full_path_name <- sprintf("%s/%s", path_name, file_name)
  } else {
    full_path_name <- sprintf("%s/%s/%s", path_name, folder, file_name)
  }
  
  # check file exists - it wont on first push
  if (!rdrop2::drop_exists(full_path_name)) {
    return(NULL)
  }
  
  # download file from drop box
  rdrop2::drop_download(
    path = full_path_name,
    local_path = here::here("dropbox_validations"),
    overwrite = TRUE
  )
  
  # reading in the log, detecting with excel or csv
  local_path <- sprintf("%s/%s", "dropbox_validations", file_name)
  
  if (str_detect(file_name, ".xls|.xlsx")) {
    df <- read_xlsx(here(local_path))
  }
  
  if (str_detect(file_name, ".csv")) {
    df <- readr::read_csv(here::here(local_path),  col_types = "iiccccccccc", na = character())
  }
  
  # this ensures the log is ordered correctly before cleaning operations in case the user
  # has sorted the data before upload. Order is important so changes are processed sequentially.
  df_out <- df |> 
    arrange(rowid) |> 
    select(-log_response_id, -rowid)
  
  return(df_out)
}
