#' Title
#'
#' @param name String. Name of rule set function e.g. create_rules_my_dataset
#' @param dir String. Name of directory where file should be created
#' @param open Logical. Should the file be opened?
#'
#' @return
#' @export
#'
#' @examples
create_rules_from_template <- function(name, dir = "/R", open = TRUE){


  template_text <- sprintf('%s <- function(){
    ## each rule should be named after the column its validating
    rule1 <- validator(
      some_column = !is.na(some_column)
    )

    ## make sure descriptions are concise and interpretable
    description(rule1) <- rep("Some description",length(rule1))
    .
    .
    .
    ruleN <- validator(
      TEST_NO = field_format(TEST_NO, "\\d+" , type="regex")
    )
    description(ruleN) <- rep("Some description",length(ruleN))

    out <- list(
      rule1 = rule1,
      .
      .
      .
      ruleN = ruleN
    )

    return(out)
  }',name)


  file_name <- sprintf("%s.R",name)
  file_path <- paste(dir,file_name,sep = "/")
  cat(template_text, file = file_path)
  if(open){
    file.edit(file_path)
  }


  return(file_path)

  status <- file.exists(here::here(file_path))


  invisible(TRUE)


}
