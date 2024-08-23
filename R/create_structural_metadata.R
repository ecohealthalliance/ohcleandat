#' Create Structural Metadata from a dataframe
#'
#' This is the metadata that describes the data themselves. This metadata can be
#' generated then joined to pre-existing metadata via field names.
#'
#'
#' @param data Any named object. Expects a table but will work
#' superficially with lists or named vectors.
#'
#' @details
#'
#' The metadata table produced has the following elements
#'
#' `name` = The name of the field. This is taken as is from `data`.
#' `description` = Description of that field. May be provided by controlled vocabulary
#' `units` = Units of measure for that field. May or may not apply
#' `term_uri` = Universal Resource Identifier for a term from a controlled vocabulary or schema
#' `comments` = Free text providing additional details about the field
#' `primary_key` = `TRUE` or `FALSE`, Uniquely identifies each record in the data
#' `foreign_key` = `TRUE` or `FALSE`, Allows for linkages between data sets. Uniquely identifies
#'  records in a different data set
#'
#'
#' @return dataframe with standard metadata requirements
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(a = 1:10, b = letters[1:10])
#' df_metadata  <- create_structural_metadata(df)
#' write.csv(df_metadata,"df_metadata.csv")
#'
#' # lets pretend we are using a dataset which already has
#' ## in airtable, you can add field descriptions directly
#' ## in the base. We want those exported and properly formatted
#' ## in our ohcleandat workflow
#'
#'  base <- "appMyBaseID"
#'  table_name <- "My Table"
#'
#'  airtable_metadata  <- airtabler::air_generate_metadata_from_api(base = base,
#'     field_names_to_snake_case = FALSE ) |>
#'     dplyr::filter(table_name == {table_name}) |>
#'     dplyr::select(field_name,field_desc,primary_key)
#'
#'  airtable_df <- airtabler::fetch_all(base = base, table_name = table_name)
#'
#'  airtable_df_metadata <- create_structural_metadata(airtable_df)
#'
#'  metadata_joined <- dplyr::left_join(airtable_df_metadata,airtable_metadata,
#'  by = c("name"="field_name"))
#'
#'  metdata_updated <- metadata_joined |>
#'  dplyr::mutate(description = field_desc,
#'                primary_key = primary_key.y,
#'                ) |>
#'  dplyr::select(-matches('\\.[xy]|field_desc'))
#'
#' # ODK
#' # get all choices from ODK form
#'
#' dotenv::load_dot_env()
#'
#' ruODK::ru_setup(
#'   svc = "https://odk.server.org/v1/projects/5/forms/myproject.svc",
#'   un = Sys.getenv("ODK_USERNAME"),
#'   pw = Sys.getenv("ODK_PASSWORD"),
#'   tz = "GMT",
#'   odkc_version = "1.1.2")
#'
#'
#' schema <- ruODK::form_schema_ext()
#'
#' schema$choices_flat <-schema$`choices_english_(en)` |>
#'   purrr::map_chr(\(x){
#'     if("labels" %in% names(x)){
#'       paste(x$labels,collapse = ", ")
#'     } else {
#'       ""
#'     }
#'
#'   })
#'
#'   data_odk <- ruODK::odata_submission_get()
#'   data_odk_rect <- ruODK::odata_submission_rectangle(data_odk)
#'   odk_metadata <- create_structural_metadata(data_odk_rect)
#'
#'
#'   odk_metadata_joined  <- dplyr::left_join(odk_metadata,schema_simple,
#'   by = c("name" = "ruodk_name"))
#'
#'   odk_metadata_choices <- odk_metadata_joined |>
#'   mutate(description = choices_flat) |>
#'   select(-choices_flat)
#'
#'
#' }
#'
create_structural_metadata <- function(data){


  # create empty data frame
  metadata <- tibble::tibble(
    name  = character(),
    description = character(),
    units = character(),
    term_uri = character(),
    comments = character(),
    primary_key = logical(),
    foreign_key = logical()
  )


  # get fields
  metadata$name = names(data)


  return(metadata)


}
