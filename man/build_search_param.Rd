% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_raw_data.R
\name{build_search_param}
\alias{build_search_param}
\title{Build Search Parameters}
\usage{
build_search_param(year, month, day, item, session_id)
}
\arguments{
\item{year}{The year for the search date.}

\item{month}{The month for the search date.}

\item{day}{The day for the search date.}

\item{item}{The 'gegenstand' item code (e.g., "0", "1", "2").}

\item{session_id}{The current session ID (jakarta.faces.ViewState).}
}
\value{
A named list of search parameters for the POST request.
}
\description{
Constructs the form parameters required for the initial search POST request.
}
