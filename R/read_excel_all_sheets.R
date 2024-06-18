#' Reads all tabs from an excel workbook
#'
#' @description
#' For a given excel file, this will detect all sheets, and iteratively read
#' all sheets and place them in a list.
#'
#' If primary keys are added, the primary key is the triplet of the file,
#' sheet name, and row number e.g. "file_xlsx_sheet1_1". Row numbering is based
#' on the data ingested into R. R automatically skips empty rows at the beginning
#' of the spreadsheet so id 1 in the primary key will belong to the first row
#' with data.
#'
#' @note The primary key method is possible because Excel forces sheet names
#'  to be unique.
#'
#' @param add_primary_key_field Logical. Should a primary key field be added?
#' @param primary_key character. The column name for the unique identifier to be added to the data.
#' @param file character. File path to an excel file
#'
#' @return list
#' @export
#'
#' @examples
#'  \dontrun{
#' # Adding primary key field
#' read_excel_all_sheet(file = "test_pk.xlsx",add_primary_key_field = TRUE)
#'
#' # Don't add primary key field
#' read_excel_all_sheet(file = "test_pk.xlsx")
#'
#'     }
#'
read_excel_all_sheets <- function(file, add_primary_key_field = FALSE, primary_key = "primary_key"){
  sheets <- readxl::excel_sheets(file)

  if(!add_primary_key_field){
    out <- purrr::map(sheets, ~readxl::read_excel(file, sheet = .x))
    return(out)
  }

  if(add_primary_key_field){
    purrr::map2(sheets,file,function(sheet,file){
      df  <- readxl::read_excel(file, sheet = sheet)

      file_name <-  gsub("\\.","_",basename(file))
      row_ids <- paste(file_name,sheet,1:nrow(df),sep = "_")

      if(primary_key%in%names(df)){

        msg <- sprintf("primary_key - %s - is already a column in the dataframe.
                     \nPlease choose a column name that isn't present in the data.",primary_key)
        rlang::abort(msg)
      }

      out <- df %>%
        dplyr::mutate({{primary_key}} := {{row_ids}})

      return(out)
    })
  }

}

