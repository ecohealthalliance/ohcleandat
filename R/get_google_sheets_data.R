#' Get Data from Google Sheets 
#'
#' @param ss Google sheet id
#'
#' @return List of dataframes from googlesheet
#' @export
#'
#' @examples
get_google_sheets_data <- function(ss){
  rvf2_sheet_names <- googlesheets4::sheet_names(ss = ss)
  # load in sheets 
  sheets_data <- purrr::map(rvf2_sheet_names, function(x){
    sheet_x <- googlesheets4::read_sheet(ss,sheet = x)
    return(sheet_x)
  })
  
  names(sheets_data) <- rvf2_sheet_names
  
  return(sheets_data)
  
}
