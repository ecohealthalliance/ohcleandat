#' Validation Correction Checks  
#' 
#' Runs some checks on data before and after validation to test expectations
#'
#' @param validation_log tibble Validation log
#' @param before_data tibble Data before corrections  
#' @param after_data tibble Data after corrections 
#' @param idcol character the primary key for the 'after_data'
#'
#' @return NULL if passed or stops with error
validation_checks <- function(validation_log, before_data, after_data, idcol){
  
  
  if(!is.null(validation_log)){
    # calculate number of assumed changes from log
    changes <- validation_log |>
      filter(
        is_valid == "FALSE" | is_valid == "F",
        !is.na(field),
        field != "",
        !is.na(entry),
        entry != "",
        new_val != ""
      ) |> 
      summarise(n = n()) |> 
      pull(n)
    
    val_fields <- validation_log |>
      filter(
        is_valid == "FALSE" | is_valid == "F",
        !is.na(field),
        field != "",
        !is.na(entry),
        entry != "",
        new_val != ""
      ) |> 
      pull(field) |> 
      unique()
    
    val_recs <- validation_log |>
      filter(
        is_valid == "FALSE" | is_valid == "F",
        !is.na(field),
        field != "",
        !is.na(entry),
        entry != "",
        new_val != ""
      ) |> 
      pull(entry) |> 
      unique()
    
    val_recs_idx <- which(pull(before_data[idcol]) %in% val_recs)
  }
  
  # perform dataframe comparison
 
  cd <- arsenal::comparedf(before_data, after_data)
  s <- summary(cd)
  
  ### TESTS
  
  # TEST: If no existing log exists > no changes are make to data
  # same vars
  # same rows
  # No unequal values
  test1 <- if(is.null(validation_log)){
    all(
      s$comparison.summary.table[s$comparison.summary.table$statistic == "Number of observations with some compared variables unequal", "value"] == 0,
      s$frame.summary.table$ncol[1] ==  s$frame.summary.table$ncol[2],
      s$frame.summary.table$nrow[1] ==  s$frame.summary.table$nrow[2]
    )
  } else {
    TRUE
  }
  
  # TEST: Log exists but no changes are recommended > No changes to data. 
  # same vars
  # same rows
  # No unequal values
  test2 <- if(!is.null(validation_log) & NROW(validation_log) == 0){
    all(
      s$comparison.summary.table[s$comparison.summary.table$statistic == "Number of values unequal", "value"] == 0,
      s$frame.summary.table$ncol[1] ==  s$frame.summary.table$ncol[2],
      s$frame.summary.table$nrow[1] ==  s$frame.summary.table$nrow[2]
    )
  } else {
    TRUE
  }
  
  # TEST: Log exists and changes recommended > number of changes are same as log
  # same vars
  # same rows
  # number of changing records in data match records in log
  test3 <- if(!is.null(validation_log) & NROW(validation_log) > 0){
    all(
      s$comparison.summary.table[s$comparison.summary.table$statistic == "Number of values unequal", "value"] == changes,
      s$frame.summary.table$ncol[1] ==  s$frame.summary.table$ncol[2],
      s$frame.summary.table$nrow[1] ==  s$frame.summary.table$nrow[2]
    )
  } else {
    TRUE
  }
  
  # TEST: Correct fields and records are being updated
  test4 <- if(!is.null(validation_log) & NROW(validation_log) > 0){
    all(
      all(s[["diffs.table"]][,1] == s[["diffs.table"]][,2]),
      all(s[["diffs.table"]][,6] == s[["diffs.table"]][,7]),
      is.null(set_diff(val_fields, s[["diffs.table"]][,1])),
      is.null(set_diff(val_fields, s[["diffs.table"]][,2])),
      is.null(set_diff(val_recs_idx,  as.character(s[["diffs.table"]][,7])))
    )
  } else {
    TRUE
  }
  
  stopifnot(
    test1, 
    test2, 
    test3,
    test4
  )
  
  message("Validation correction checks completed without error.")
  
  return(s)
  
}


