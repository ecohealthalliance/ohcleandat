#' Expand Frictionless Metadata with structural metadata
#'
#' Loops over elements in the structural metadata and adds them to frictionless
#' metadata schema.
#'
#' @param structural_metadata Dataframe. Structural metadata from
#' `create_structural_metadata` or `update_structural_metadata`
#' @param resource_name Character. Item within the datapackage to be updated
#' @param resource_path Character. Path to csv file
#' @param data_package_path Character. Path to datapackage.json file
#'
#' @return Updates the datapackage, returns nothing
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # read in file
#' data_path <- "my/data.csv"
#' data <- read.csv(data_path)
#'
#' # create structural metadata
#' data_codebook  <- create_structural_metadata(data)
#'
#' # update structural metadata
#' write.csv(data_codebook,"my/codebook.csv", row.names = FALSE)
#'
#' data_codebook_updated <- read.csv(""my/codebook.csv"")
#'
#' # create frictionless package - this is done automatically with the
#' # deposits package
#' my_package <-
#'  create_package() |>
#'  add_resource(resource_name = "data", data = data_path)
#'
#'  write_package(my_package,"my")
#'
#' expand_frictionless_metadata(structural_metadata = data_codebook_updated,
#'                             resource_name = "data",
#'                             resource_path = data_path,
#'                             data_package_path = "my/datapackage.json"
#'                             )
#'
#' }
#'
expand_frictionless_metadata <- function(structural_metadata,
                                         resource_name,
                                         resource_path,
                                         data_package_path ){

  data_package <- frictionless::read_package(data_package_path)

  data_package_dir <- dirname(data_package_path)

  # get the schema for a resource in the data package
  my_data_schema <- data_package|>
    frictionless::get_schema(resource_name)

  ## build up schema based on structural metadata

  for(idx in 1:length(my_data_schema$fields)){
    # item to build out
    x <- my_data_schema$fields[[idx]]
    for(idy in 1:length(structural_metadata)){

      y <- structural_metadata[idx,idy][[1]]
      # get property name
      property_to_add_name <- names(structural_metadata)[idy]

      # skip properties that already exist
      if(property_to_add_name %in% names(x)){
        next()
      }

      property_to_add_value <- y
      names(property_to_add_value) <- property_to_add_name
      x <- c(x, property_to_add_value)
    }

    # update
    my_data_schema$fields[[idx]] <- x
  }

  # update the datapackage.json
  data_package <- data_package|>
    frictionless::remove_resource(resource_name) |>
    frictionless::add_resource(resource_name = resource_name,
                               data = resource_path,
                               schema = my_data_schema,
    )

  # write the datapackage.json
  frictionless::write_package(data_package,directory = data_package_dir)

  invisible()
}
