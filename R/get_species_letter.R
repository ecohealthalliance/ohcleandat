#' Get Species Letter
#'
#' This function maps the relationship between animal species and hum_anim_id codes.
#' This is for use in id_checker()
#'
#' @param species character The species identifier. See argument options
#' @export
#'
#' @return character The hum_anim_id code
get_species_letter <- function(species = c("human", "cattle", "small_mammal", "sheep", "goat")){

  if(missing(species)){stop("species must not be missing. See get_species_letter() for options.")}

  x <- match.arg(species)

  switch(x,
         "human" = "H",
         "cattle" = "C",
         "small_mammal" = "SM",
         "sheep" = "S",
         "goat" = "G"
  )

}
