#' Get ODK Questionnaire Response Data
#'
#' This function handles the authentication and pulling of submission
#' data for ODK Questionnaires. 
#'
#' @param url character The survey URL
#' @param un character The ODK account username
#' @param pw character The ODK account password
#'
#' @return data.frame of flattened survey responses
#'
get_odk_responses <-
  function(url,
           un = Sys.getenv("ODK_USERNAME"),
           pw = Sys.getenv("ODK_PASSWORD")) {
    # ODK  Auth
    ruODK::ru_setup(svc = url,
                    un = un,
                    pw = pw,
                    tz = "UTC",
                    odkc_version = Sys.getenv("ODKC_VERSION"))
    
    # get responses
    responses <-
      ruODK::odata_submission_get(odkc_version = Sys.getenv("ODKC_VERSION"), parse = F, wkt = TRUE) |>
      ruODK::odata_submission_rectangle(names_sep = NULL)
    
    return(responses)
    
  }
