#' Create Translation Log
#'
#' Collates free text responses from 'other' and 'notes' fields in the survey
#' data. Some language detection is performed, and those records that are not
#' detected as english are returned in a validation log for possible translation.
#'
#' @param response_data data.frame of ODK questionnaire responses
#' @param form_schema data.frame od flattened ODK form schema
#' @param url The ODK submission URL excluding the uuid identifier
#'
#' @return data.frame validation log
#'
create_translation_log <- function(response_data, form_schema, url) {
  
  other_q <- form_schema |>
    select(name, type, labels = `label_english_(en)`, choices = `choices_english_(en)`) |>
    filter(str_detect(labels, "other|Other|note|Note"),
           choices == "NA",
           type == "string") |>
    pull(name) |>
    unique()
  
  other_responses <- response_data |>
    select(id, contains(other_q)) |>
    pivot_longer(-id) |>
    filter(!is.na(value)) |>
    mutate(comments = map_chr(value, detect_language))
  
  trans_log <- other_responses |>
    mutate(
      is_valid = "",
      new_val = "",
      user_initials = "",
      issue = "Free-text detected. Review and translate if required.",
      odk_url = paste(url, str_replace(id, pattern = ":", replacement = "%3A"), sep = "/"),
      overwrite_old_value = "FALSE"
    ) |> 
    left_join(
      select(form_schema, name, question = "label_english_(en)") , by = c("name" = "name")
    ) |> 
    select(
      entry = id,
      field = name,
      question,
      issue,
      old_value = value,
      is_valid,
      new_val,
      overwrite_old_value,
      user_initials,
      odk_url,
      comments
    )
  
  
  return(trans_log)
  
}


