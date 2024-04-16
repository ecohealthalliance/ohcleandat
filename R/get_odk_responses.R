#' Get ODK Questionnaire Response Data
#'
#' @description
#' This function handles the authentication and pulling of responses
#' data for ODK Questionnaires. The raw return list is 'rectangularized' into
#' a data frame first. See the `ruODK` package for more info on how this happens.
#'
#' @details
#' This is a wrapper around the `ruODK` package. It handles the setup and
#' authentication. See \url{https://github.com/ropensci/ruODK}
#'
#' @param url character The survey URL
#' @param un character The ODK account username
#' @param pw character The ODK account password
#' @param odkc_version character The ODK version
#'
#' @return data.frame of flattened survey responses
#' @export
#' @seealso [ruODK::form_schema_ext()]
#' @examples
#' \dontrun{
#'     get_odk_responses(url ="https://odk.eha.io/v1/projects/5/forms/RVF2_animal_owner.svc",
#'     un = Sys.getenv("ODK_USERNAME"),
#'     pw = Sys.getenv("ODK_PASSWORD"),
#'     odkc_version = Sys.getenv("ODKC_VERSION"))
#' }
get_odk_responses <-
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

    # get responses
    responses <-
      ruODK::odata_submission_get(odkc_version = odkc_version, parse = F, wkt = TRUE) |>
      ruODK::odata_submission_rectangle(names_sep = NULL)

    return(responses)

  }
