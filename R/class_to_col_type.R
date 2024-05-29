#' Class to Column Type lookup table
#'
#' A table that links classes to `readr` column types.
#' Created from csv file of the same name in inst/
#'
#' class_to_col_type <- read.csv(file = "inst/class_to_col_type.csv")
#' usethis::use_data(class_to_col_type,overwrite = TRUE)
#'
#' @format ## `class_to_col_type`
#' A data frame with 9 rows and 3 columns:
#' \describe{
#'   \item{col_type}{Type of column as described in `readr`}
#'   \item{col_class}{Class of R object that matches that column type}
#'   \item{col_abv}{Abbreviation for that column type from `reader`}
#'   ...
#' }
#' @seealso [reader::cols()]
"class_to_col_type"
