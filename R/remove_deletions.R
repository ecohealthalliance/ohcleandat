#' Utility function to identify records for deletion
#'
#' @description
#' Filters for records matching a given string.
#'
#' @details
#' To be used within `dplyr::filter()`. The function returns a logical vector
#' with TRUE resulting from values that are not equal to the `val` argument. Also
#' protects from NA values.
#'
#' Used within verbs such as `tidyselect::all_of()` this can work effectively across all
#' columns in a data frame. See examples
#'
#' @param x input vector
#' @param val The value to check for inequality. Defaults to 'Delete'
#'
#' @return logical vector
#' @export
#'
#' @examples
#' \dontrun{
#' data |> filter(if_all(everything(), remove_deletions))
#' }
remove_deletions <- function(x, val = "Delete"){
  x != val | is.na(x)
}
