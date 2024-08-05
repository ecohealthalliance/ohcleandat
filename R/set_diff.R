#' Get items that differ between x and y
#'
#' Unlike setdiff, this function creates the union of x and y then
#' removes values that are in the intersect, providing values
#' that are unique to X and values that are unique to Y.
#'
#' @param x a set of values.
#' @param y a set of values.
#'
#' @return Unique values from X and Y, NULL if no unique values.
#' @export
#'
#' @examples
#' a <- 1:3
#' b <- 2:4
#'
#' set_diff(a,b)
#' # returns 1,4
#'
#' x <- 1:3
#' y <- 1:3
#'
#' set_diff(x,y)
#' # returns NULL
#'
set_diff <- function(x,y){
  u <- union(x,y)
  i <- intersect(x,y)
  j <- (u %in% i)

  if(all(j)){
    return(NULL)
  }

  diff <- u[!(j)]
  return(diff)
}
