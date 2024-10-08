% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_frictionless_metadata.R
\name{update_frictionless_metadata}
\alias{update_frictionless_metadata}
\title{Update descriptive metadata in frictionless datapackage}
\usage{
update_frictionless_metadata(descriptive_metadata, data_package_path)
}
\arguments{
\item{descriptive_metadata}{List of descriptive metadata terms.}

\item{data_package_path}{Character. Path to datapackage.json file}
}
\value{
invisibly writes datapackage.json
}
\description{
This function overwrites the descriptive metadata associated with a
frictionless datapackage. It does \emph{NOT} validate the metadata, or check
for conflicts with existing descriptive metadata. It is very easy to create
invalid metadata.
}
\examples{
\dontrun{
descriptive_metadata <- list (
title = "Example Dataset",
description = "This is the abstract but it needs more detail",
creator = list (list (name = "A. Person"), list (name = "B. Person"),
list (name = "C. Person"),list (name = "F. Person"))
# , accessRights = "open"
)
update_frictionless_metadata(descriptive_metadata = descriptive_metadata,
                             data_package_path = "data_examples/datapackage.json"
)
}
}
