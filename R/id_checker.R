#' ID Checker
#'
#' General function for checking and correcting ID columns.
#'
#' In order to use the autobot process for correcting ID columns, a new 'corrected'
#' column is created by the user using the id_checker() function. It will take an
#' existing vector of ID's, and an ID type (animal, mosquito, etc) and apply the
#' bespoke corrections. This can then be consumed by the autobot log.
#'
#' @param col The vector of ID's to be checked
#' @param type The ID type, see argument options for allowable settings
#' @param ... other function arguments passed to get_species_letter
#'
#' @return vector of corrected ID's
#' @export
#'
#' @examples
#' \dontrun{
#' # with a species identifier
#'     data |> mutate(animal_id_new = id_checker(animal_id, type = "animal", species = "cattle"))
#'     data |> mutate(farm_id_new = id_checker(farm_id, type = "site"))
#'     }
id_checker  <- function(col, type = c("animal", "hum_anim", "site"), ...){

  if(missing(type)){stop("type must not be missing")}

  type <- match.arg(type, c("animal", "hum_anim", "site"))

  # animal ID checks
  if(type == "animal") {
    # checks the characters of the hyphen and pads to the correct length with zeros
    correction <- paste0(
      stringr::str_extract({{col}}, "^.+\\-"), # all characters from the start of the ID to the hyphen
      stringr::str_pad(
        stringr::str_extract({{col}}, "(?<=\\-).+$"), # all character from after the hyphen to the end
        side = "left",
        pad = "0",
        width = 4
      )
    )
    # post corrections this checks the ID is formatted correctly, else sets to NA for manual flagging
    new <- ifelse(stringr::str_detect(correction, "^KZNRVF(22|23)(C|G|S)\\-\\d{4}"), correction, NA)
  }

  # Human and Animal ID checks
  if(type == "hum_anim"){

    # should be upper case
    c1 <- toupper({{col}})

    # species code should match pattern based on what type of animal
    c2 <- ifelse(stringr::str_extract(c1, "^\\D+") == get_species_letter(...), c1, NA)

    # replace O with 0 in numeric section
    c3 <- stringr::str_replace(c2, pattern = "O(?=.{0,3}$)", replacement = "0")

    # left pad numeric part with 0
    c4 <- stringr::str_replace(c3, pattern = "\\d+$", stringr::str_pad(stringr::str_extract(c3, "\\d+$"), side = "left", pad = "0", width = 4))

    # Check anything that is "0000" manually
    c5 <- ifelse(stringr::str_detect(c4, "0{4}$"), NA, c4)

    # final format check
    new <- ifelse(stringr::str_detect(c5, "^(H|C|S|G|SM)\\d{4}$"), c5, NA)

  }

  # SiteID checks
  if(type == "site"){

    # trim white space
    c1 <- stringr::str_trim({{col}})

    # to upper case
    c2 <- toupper(c1)

    # 2. correct o's in first numeric part
    c3 <- stringr::str_replace(c2, pattern = "(?<=^.{0,2})O", replacement = "0")

    # 3. numbers padded with zero to increase to 3 characters
    c4 <- stringr::str_replace(c3, pattern = "^\\d+", stringr::str_pad(stringr::str_extract(c3, "^\\d+"), side = "left", pad = "0", width = 3))

    # Format check
    new <- ifelse(stringr::str_detect(c4, "^\\d{3}.{3}\\d{3}$"), c4, NA)
  }

  return(new)

}



