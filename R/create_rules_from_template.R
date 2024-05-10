#' Title
#'
#' @param name String. Name of rule set function e.g. create_rules_my_dataset
#' @param dir String. Name of directory where file should be created. If it
#' doesnt exist, a folder will be created.
#' @param open Logical. Should the file be opened?
#' @param showWarnings Logical. Should dir.create show warnings?
#'
#' @return String. File path of newly created file
#' @export create_rules_from_template
#'
#' @examples
#' \dontrun{
#'     # create a ruleset and immediately open it
#'     create_rules_from_template(name = "create_rules_field_data")
#'     # create a ruleset and don't open it
#'     create_rules_from_template(name = "create_rules_lab_data", open = FALSE)
#'     # create a ruleset and store it in a different folder
#'     create_rules_from_template(name = "create_rules_lab_data",
#'     dir = "/path/to/rulesets" open = FALSE)
#'     }
create_rules_from_template <- function(name, dir = "/R", open = TRUE, showWarnings = FALSE){

  dir.create(here::here("/R"),showWarnings = showWarnings, recursive = TRUE)

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
    utils::file.edit(file_path)
  }


  return(file_path)

  status <- file.exists(here::here(file_path))

  if(!status){
    rlang::abort(sprintf("%s not created",file_path))
  }

  return(file_path)

}
