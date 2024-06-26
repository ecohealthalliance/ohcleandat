#' Reads all tabs from an excel workbook
#'
#' @description
#' For a given excel file, this will detect all sheets, and iteratively read
#' all sheets and place them in a list.
#'
#' @param file character File path to an excel file
#'
#' @return list
#' @export
read_excel_all_sheets <- function(file){
  sheets <- readxl::excel_sheets(file)
  purrr::map(sheets, ~readxl::read_excel(file, sheet = .x))
}
