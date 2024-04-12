#' Create Free Text Log
#'
#' Creates custom validation log for 'other: explain' free text responses that may contain valid
#' multi-choice options.
#'
#' @param response_data data.frame ODK questionnaire response data
#' @param form_schema data.frame ODK flattened form schema data
#' @param url The ODK submission URL excluding the uuid identifier
#' @param ... other arguments passed to `othertext_lookup()`
#'
#' @return data.frame validation log
#' @export
create_freetext_log <- function(response_data, form_schema, url, ...){

  # identify questions with some free text response
  other_q <- form_schema |>
    dplyr::select(name, type, labels = `label_english_(en)`, choices = `choices_english_(en)`) |>
    dplyr::filter(stringr::str_detect(labels, "other|Other|note|Note"),
           choices == "NA",
           type == "string") |>
    dplyr::pull(name) |>
    unique()

  # identidy responses to the questions with free text responses
  other_responses <- response_data |>
    dplyr::select(id, tidyselect::contains(other_q)) |>
    tidyr::pivot_longer(-id) |>
    dplyr::filter(!is.na(value))

  # Identify questions with multi-response options
  multi <- form_schema |>
    dplyr::select(name, type, labels = `label_english_(en)`, choices = `choices_english_(en)`) |>
    dplyr::filter(choices != "NA" & choices != "NULL")

  # explode label options
  multi_other_lst <- rlang::set_names(multi$choices, multi$name)
  multi_options <- purrr::map_dfr(multi_other_lst, dplyr::bind_rows, .id = "name")

  # Read in pre-defined lookup of free text responses and the base multi-option question.
  # Join these with actual responses to form a validation log
  freetext_log <- othertext_lookup(...) |>
    dplyr::inner_join(multi_options, by = dplyr::join_by(name)) |>
    dplyr::inner_join(other_responses, by = dplyr::join_by(other_name == name)) |>
    dplyr::mutate(issue = "Is the free-text answer valid? Indicate IsValid = F to overwrite with the correct multiple choice response",
           is_valid = "",
           user_initials = "",
           odk_url = paste(url, stringr::str_replace(id, pattern = ":", replacement = "%3A"), sep = "/"),
           overwrite_old_value = "FALSE",
           comments = ifelse(stringr::str_detect(string = tolower(value), pattern = tolower(values)), "Text contains a valid multiple choice option.", "")) |>
    dplyr::left_join(
      dplyr::select(form_schema, name, question = "label_english_(en)") , by = c("name" = "name")
    ) |>
    dplyr::select(entry = id,
           field = name,
           question,
           issue,
           old_value = value,
           is_valid,
           new_val = values,
           overwrite_old_value,
           user_initials,
           odk_url,
           comments) |>
    dplyr::arrange(entry, field)

  return(freetext_log)

}
