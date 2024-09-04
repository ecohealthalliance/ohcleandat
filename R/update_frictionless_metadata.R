#' Title
#'
#' @param descriptive_metadata List of descriptive metadata terms.
#' @param data_package_path Character. Path to datapackage.json file
#'
#' @return invisibly writes datapackage.json
#' @export
#'
#' @examples
#' \dontrun{
#' descriptive_metadata <- list (
#' title = "Example Dataset",
#' description = "This is the abstract but it needs more detail",
#' creator = list (list (name = "A. Person"), list (name = "B. Person"),
#' list (name = "C. Person"),list (name = "F. Person"))
#' # , accessRights = "open"
#' )
#' update_frictionless_metadata(descriptive_metadata = descriptive_metadata,
#'                              data_package_path = "data_examples/datapackage.json"
#' )
#' }
update_frictionless_metadata <- function(descriptive_metadata,
                                         data_package_path){

  data_package <- frictionless::read_package(data_package_path)

  data_package_dir <- dirname(data_package_path)

  data_package$metadata <- descriptive_metadata

  # write the datapackage.json
  frictionless::write_package(data_package,directory = data_package_dir)

  invisible()
}
