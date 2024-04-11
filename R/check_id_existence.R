#' Check existence of ID columns across two tables
#'
#' This returns rows in x without a match in y. Returning selected columns only. It
#' is a this wrapper around `dplyr::anti_join`.
#'
#' @param x data.frame or tibble containing match id to check for non existence in y
#' @param y data.frame or tibble to check for non-existence of match id from x
#' @param by character containing match id, or if named different, a named character vector like c("a" = "b")
#' @param select_cols character vector of columns to select in the output. Note that during the join, columns with identical names in both data sets will have a suffix of .x or .y added to disambiguate. These need to be added to ensur the correct column is returned.
#' @param ... other variables passed to dplyr::anti_join
#'
#' @export
#'
#' @return tibble rows from x without a match in y
#'
#' @examples \dontrun{
#' check_id_existence(x,
#'                    y,
#'                    by =  c("Batch_ID" = "batch_id"),
#'                    select_cols = c("Batch_ID", "iDate", "Farm_ID"))
#' }
#' @seealso `dplyr::anti_join`
check_id_existence <- function(x, y, by, select_cols, ...){
  dplyr::anti_join(x, y, by, ...) |>
    dplyr::select(tidyselect::all_of(select_cols))

}


