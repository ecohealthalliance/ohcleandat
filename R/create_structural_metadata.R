#' Create Structural Metadata from a dataframe
#'
#' This is the metadata that describes the data themselves. This metadata can be
#' generated then joined to pre-existing metadata via field names.
#'
#'
#' @param data Any named object. Expects a table but will work
#' superficially with lists or named vectors.
#' @param primary_key Character. name of field that serves as a primary key
#' @param foreign_key Character. Field or fields that are foreign keys
#' @param additional_elements Empty tibble with structural metadata elements and
#' their types.
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
#' df_metadata  <- ohcleandat::create_structural_metadata(df)
#' write.csv(df_metadata,"df_metadata.csv")
#'
#'
#' Additional elements can be added via a tibble
#' additional_elements <- tibble::tibble(table_name = NA_character_,
#' created_by = NA_character_,
#' updated = NA
#' )
#' df_metadata  <- ohcleandat::create_structural_metadata(df,
#'     additional_elements = additional_elements)
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
#'  airtable_df_metadata <- ohcleandat::create_structural_metadata(airtable_df)
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
#'   odk_metadata <- ohcleandat::create_structural_metadata(data_odk_rect)
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
create_structural_metadata <- function(data,
                                       primary_key = "",
                                       foreign_key= "",
                                       additional_elements = tibble::tibble()){


  # create empty data frame
  metadata <- tibble::tibble(
    name  = names(data),
    description = NA_character_,
    units = NA_character_,
    term_uri = NA_character_,
    comments = NA_character_,
    primary_key = FALSE,
    foreign_key = FALSE
  )

  if(nrow(additional_elements) > 0){

    # check that elements aren't repeated
    if(any(names(additional_elements) %in% names(metadata))) {
      names_index  <- which(names(additional_elements) %in% names(metadata))
      repeat_names <- paste(names(additional_elements)[names_index],collapse = ", ")
      msg <- sprintf("additional_elements repeats the following fields: %s",additional_elements)
      rlang::abort(msg)
    }
    metadata<- cbind(metadata,additional_elements)
  }

  if(nzchar(primary_key)){

    pkey_check <- metadata$name == primary_key

    if(any(pkey_check)){
      metadata[which(pkey_check),"primary_key"] <- TRUE
    } else {
      sprintf("Primary key not in data: %s", primary_key)
    }
  }

  if(any(nzchar(foreign_key))){

    fkey_check <- metadata$name %in% foreign_key

    if(any(fkey_check)){
      metadata[which(fkey_check),"foreign_key"] <- TRUE
    } else {
      sprintf("Foreign key not in data: %s", foreign_key)
    }
  }


  return(metadata)

}


#' Update structural metadata
#'
#' Appends rows and/or columns to existing metadata, change primary key and/or
#' adds foreign keys.
#'
#' @param data Any named object. Expects a table but will work
#' superficially with lists or named vectors.
#' @param metadata Data frame. Output from `create_structural_metadata`
#' @param primary_key Character. OPTIONAL Primary key in the data
#' @param foreign_key Character. OPTIONAL Foreign key or keys in the data
#' @param additional_elements data frame. OPTIONAL Empty tibble with structural
#' metadata elements and their types.
#'
#' @note See vignette on metadata for examples
#'
#' @return data.frame
#' @export
update_structural_metadata <- function(data,metadata,primary_key = "", foreign_key = "",additional_elements = tibble::tibble()){

  existing_fkeys <- metadata |>
    dplyr::filter(foreign_key) |>
    dplyr::pull(name)

  if(rlang::is_empty(existing_fkeys)){
    existing_fkeys <- ""
  }

  if(any(nzchar(foreign_key))){
    foreign_key <- c(foreign_key,existing_fkeys)
  } else {
    foreign_key <- existing_fkeys

  }

  if(!nzchar(primary_key)){
    primary_key <-    metadata |>
      dplyr::filter(primary_key) |>
      dplyr::pull(name)
  }

  # make new structural metadata
  new_sm <- ohcleandat::create_structural_metadata(data,
                                                   additional_elements = additional_elements,
                                                   primary_key = primary_key ,
                                                   foreign_key= foreign_key)

  # cols to add old metadata
  if(nrow(additional_elements) > 0){

    # check that elements aren't repeated
    if(any(names(additional_elements) %in% names(metadata))) {
      names_index  <- which(names(additional_elements) %in% names(metadata))
      repeat_names <- paste(names(additional_elements)[names_index],collapse = ", ")
      msg <- sprintf("additional_elements repeats the following fields: %s",additional_elements)
      rlang::abort(msg)
    }
    metadata<- cbind(metadata,additional_elements)
  }

  # rows to append
  data_to_append <- dplyr::anti_join(new_sm,metadata,"name")

  # rows to drop
  rows_to_drop <- dplyr::anti_join(metadata,new_sm,"name")

  metadata_full <- rbind(metadata, data_to_append)

  if(nrow(rows_to_drop) > 0){
     name_filter <- rows_to_drop |>
      dplyr::pull(name)

     metadata_full <-   metadata_full |>
       dplyr::filter(!name %in% {name_filter})

  }


  # update keys
  metadata_full$primary_key <- new_sm$primary_key
  metadata_full$foreign_key <- new_sm$foreign_key

  return(metadata_full)

}

