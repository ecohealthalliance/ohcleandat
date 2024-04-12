#' Detect Language  
#' 
#' A function that extracts the top guess to the language of a piece of text. 
#' 
#' Utilizes the stringi package encoding detector as the means to infer languange. 
#'
#' @param text character any text string 
#'
#' @return character estimate for language abbreviation
detect_language <- function(text){
  enc <- stringi::stri_enc_detect(text)  
  enc[[1]][["Language"]][1]
}