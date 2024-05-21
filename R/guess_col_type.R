#' Guess the column type
#'
#' uses column class to set readr column type
#'
#' @param data data.frame Data who column types you would like to guess
#' @param default_col_abv string. Column type abbreviation from [readr::cols()].
#' Use "g" to guess the column type.
#'
#' @return character vector of column abbreviations
#' @export
#'
#' @examples
#' data <- data.frame(time = Sys.time(),
#' char = "hello", num = 1, log = TRUE,
#' date = Sys.Date(), list_col = list("hello") )
#'
#' guess_col_type(data)
#'
#' ## change default value of default column abbreviation
#'
#' guess_col_type(data, default_col_abv = "g")
#'
#'
guess_col_type <- function(data, default_col_abv = "c"){

  ## get column class
   purrr::map_chr(data, function(x){
    class_x  <- data.frame(col_class = class(x))

    ### what if there are no matches?
    class_df <- dplyr::left_join(class_x,ohcleandat::class_to_col_type,"col_class")

    col_abv <- dplyr::distinct(class_df,col_abv ) |>
                dplyr::pull(col_abv)

    na_check <- is.na(col_abv)

    if(all(na_check)){
      class_for_msg <- paste(class(x),collapse = ", ")
      msg <- sprintf("column type %s might not be supported, defaulting to %s", class_for_msg,default_col_abv)
      rlang::warn(msg)
      return(default_col_abv)
    }

    if(any(na_check)){
      # use non na class(es)
      col_abv <- col_abv[which(!na_check)]
    }

    if(length(col_abv) > 1){
      rlang::warn("column has multiple classes of distinct types,
              using first class as column type")
      col_abv <- col_abv[1]
    }

    return(col_abv)
   })

}
