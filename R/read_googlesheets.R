#' Read Google Sheets Data
#'
#' For a given sheet id, this handles authentication and reads in a specified sheet, or all sheets.
#'
#' @param key_path character path to google authentication key
#' @param sheet Sheet to read, in the sense of "worksheet" or "tab".
#' @param ss Something that identifies a Google Sheet such as drive id or URL
#' @param ... other arguments passed to googlesheets4::range_read
#'
#' @return tibble
#'
read_googlesheets <-
  function(key_path = here::here("./auth/rvf2-workflow-automation-2877f538ddb9.json"),
           sheet = "all",
           ss,
           ...) {
    # Handle authentication
    googledrive::drive_auth(path = key_path)
    googlesheets4::gs4_auth(path = key_path)
    
    # decide of all sheets are read and combined, or just one.
    if (sheet == "all") {
      sheet_names <- googlesheets4::sheet_names(ss = ss)
      sheet_names <- set_names(sheet_names, sheet_names)
      dat <-
        purrr::map(sheet_names,
                       \(x) googlesheets4::range_read(ss = ss, sheet = x, na = c("", "NA", "NULL", "-"), ...))
      
    } else{
      dat <- googlesheets4::range_read(ss = ss, sheet = sheet, na = c("", "NA", "NULL", "-"), ...)
      
    }
    
    return(dat)
    
  }
