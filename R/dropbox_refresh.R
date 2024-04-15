#' Dropbox Refresh
#'
#' This is a helper function to trigger the auto-refresh of a dropbox token
#'
#' @details
#' This is not an exported function and is only called in download_dropbox() as
#' a means of refreshing a token. It works by forcing the download of a small sample
#' file from a fixed file path on EcoHealth drop box.
#'
#' @param path to drop box token `.rds` object
#'
#' @return invisible
#'
refresh_db_token <- function(path = "auth/db_token.rds") {
  rdrop2::drop_auth(rdstoken = path, cache = FALSE)
  # download a small file to get the token to refresh properly. Allows upload functions to work.
  rdrop2::drop_download(
    "/dtra_rvf2/data/rvf2_github_data/tinyfile.txt",
    local_path = here::here("auth"),
    overwrite = TRUE
  )
  file.remove(here::here("auth/tinyfile.txt"))
}
