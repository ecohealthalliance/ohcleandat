#' Create Validation Log
#'
#' @param data data fame  Input data to be validated
#' @param rule_set  a rule set of class validator from the validate package
#' @param pkey character  A character vector giving the column name of the primary key or unique row identifier in the data
#' @param ... other arguments passed to validate::confront
#'
#' @return a data frame formatted as a validation log for human review
#' @export
#'
create_validation_log <- function(data, pkey, rule_set, ...) {
    
    conf_obj <- validate::confront(data, rule_set, raise = 'all', ...)
    
    rule_sum <- summary(conf_obj) |>
      cbind(description = meta(rule_set)[["description"]]) |>
      mutate(
        description = na_if(description, ""),
        description = coalesce(description, expression)
      )
    
    rule_vals <- validate::values(conf_obj)
    
    issues <- rule_vals |>
      as_tibble() |>
      mutate(id = pull(data, pkey), .before = 1) |>
      pivot_longer(cols = -id)  |>
      filter(value == FALSE) |>
      inner_join(rule_sum, by = join_by(name)) |>
      select(entry = id,
             field = name,
             issue = description) |>
      mutate(field = str_extract(field, pattern = "^.*?(?=\\.(\\d+)|$)")) |>
      left_join(
        data,
        by = c("entry" = pkey),
        na_matches = "never",
        keep = TRUE
      ) 
    
    log <- issues |>
      mutate(across(everything(), as.character)) |>
      pivot_longer(-c(entry, field, issue),
                   values_to = "old_value",
                   values_transform = as.character) |>
      filter(field == name) |>
      select(-name) |>
      replace_na(list(old_value = '')) |>
      mutate(
        is_valid = '',
        new_val = '',
        user_initials = '',
        comments = '',
      ) |> 
      select(entry, field, issue, old_value, is_valid, new_val, user_initials, comments)
    
    return(log)

  }
