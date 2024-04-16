#' Get ODK Questionnaire Schema Info
#'
#' @description
#' This function handles the authentication and pulling of questionnaire
#' form schema information.
#'
#' @details
#' This is a wrapper around the `ruODK` package. It handles the setup and
#' authentication. See \url{https://github.com/ropensci/ruODK}
#'
#' @param url character The survey URL
#' @param un character The ODK account username
#' @param pw character The ODK account password
#' @param odkc_version character The ODKC Version string
#'
#' @return data frame of survey responses
#' @export
#' @seealso [ruODK::form_schema_ext()]
#' @examples
#' \dontrun{
#'     get_odk_form_schema(url ="https://odk.eha.io/v1/projects/5/forms/RVF2_animal_owner.svc",
#'     un = Sys.getenv("ODK_USERNAME"),
#'     pw = Sys.getenv("ODK_PASSWORD"),
#'     odkc_version = Sys.getenv("ODKC_VERSION"))
#' }
#'
get_odk_form_schema <-
  function(url,
           un = Sys.getenv("ODK_USERNAME"),
           pw = Sys.getenv("ODK_PASSWORD"),
           odkc_version = Sys.getenv("ODKC_VERSION")) {
    # ODK  Auth
    ruODK::ru_setup(
      svc = url,
      un = un,
      pw = pw,
      tz = "UTC",
      odkc_version = odkc_version
    )

    # get form schema
    schema <- ruODK::form_schema_ext(odkc_version = odkc_version)

    schema$name <- tolower(schema$name)

    return(schema)

  }
