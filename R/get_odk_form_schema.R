#' Get ODK Questionnaire Schema Info
#'
#' This function handles the authentication and pulling of questionnaire
#' form schema information.
#'
#' @param url character The survey URL
#' @param un character The ODK account username
#' @param pw character The ODK account password
#'
#' @return data frame of survey responses
#'
get_odk_form_schema <-
  function(url,
           un = Sys.getenv("ODK_USERNAME"),
           pw = Sys.getenv("ODK_PASSWORD")) {
    
    # ODK  Auth
    ruODK::ru_setup(
      svc = url,
      un = un,
      pw = pw,
      tz = "UTC",
      odkc_version = Sys.getenv("ODKC_VERSION")
    )
    
    # get form schema
    schema <- ruODK::form_schema_ext(odkc_version = Sys.getenv("ODKC_VERSION"))
    
    schema$name <- tolower(schema$name)
    
    return(schema)
    
  }
