% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_odk_responses.R
\name{get_odk_responses}
\alias{get_odk_responses}
\title{Get ODK Questionnaire Response Data}
\usage{
get_odk_responses(
  url,
  un = Sys.getenv("ODK_USERNAME"),
  pw = Sys.getenv("ODK_PASSWORD"),
  odkc_version = Sys.getenv("ODKC_VERSION")
)
}
\arguments{
\item{url}{character The survey URL}

\item{un}{character The ODK account username}

\item{pw}{character The ODK account password}

\item{odkc_version}{character The ODK version}
}
\value{
data.frame of flattened survey responses
}
\description{
This function handles the authentication and pulling of responses
data for ODK Questionnaires. The raw return list is 'rectangularized' into
a data frame first. See the \code{ruODK} package for more info on how this happens.
}
\details{
This is a wrapper around the \code{ruODK} package. It handles the setup and
authentication. See \url{https://github.com/ropensci/ruODK}
}
\examples{
\dontrun{
    get_odk_responses(url ="https://odk.xyz.io/v1/projects/5/forms/survey.svc",
    un = Sys.getenv("ODK_USERNAME"),
    pw = Sys.getenv("ODK_PASSWORD"),
    odkc_version = Sys.getenv("ODKC_VERSION"))
}
}
\seealso{
\code{\link[ruODK:form_schema_ext]{ruODK::form_schema_ext()}}
}
