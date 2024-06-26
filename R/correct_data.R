#' Correct data using validation log
#'
#' Takes a validation log and applies the required changes to the data
#'
#' @param validation_log tibble a validation log
#' @param data tibble the original unclean data
#' @param primary_key character the quoted column name for the unique identifier in data
#'
#' @return tibble the semi-clean data set
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @export
correct_data <- function(validation_log, data, primary_key){

  # if the log returns NULL (for example there is no existing log), then return raw data
  if(is.null(validation_log)){
    return(data)
  }

  # keeping only records for correction (is_valid = F) and removing NA or blank required info
  validation_log <- validation_log |>
    dplyr::filter(
      is_valid == "FALSE" | is_valid == "F",
      !is.na(field),
      field != "",
      !is.na(entry),
      entry != "",
      new_val != ""
    )

  # if nothing to correct return raw data
  if(nrow(validation_log) == 0){
    return(data)
  }

  # preserve col types. The log deals only in character types so need to fix later
  ## need to map out readr col types to different classes

  col_types <- paste(guess_col_type(data), collapse = "")

  # transform to character for imputation
  dat_chr <- data |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))

  # main workhorse loop
  for(i in 1:nrow(validation_log)){

    col_nme <- validation_log$field[i]
    row_id <- validation_log$entry[i]
    old <- validation_log$old_value[i]
    new <- validation_log$new_val[i]
    overwrite <- ifelse("overwrite_old_value" %in% names(validation_log), validation_log$overwrite_old_value[i], "TRUE")

    dat_chr <- dat_chr %>%
      dplyr::mutate(
        !!rlang::sym(col_nme) := dplyr::case_when(
          (!!rlang::sym(primary_key) == !!row_id) & overwrite == "FALSE" ~ paste(!!rlang::sym(col_nme), !!new),
          (!!rlang::sym(primary_key) == !!row_id) & overwrite == "TRUE"  ~  !!new,
          TRUE ~ !!rlang::sym(col_nme)
        )
      )


  }

  # back convert to original types
  dat_out <- readr::type_convert(dat_chr, col_types, na = character())

  return(dat_out)
}
