% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replext_helpers.R
\name{handle_null}
\alias{handle_null}
\title{Handle NULL or Empty Input Parameters}
\usage{
handle_null(par_input = "")
}
\arguments{
\item{par_input}{An input parameter that could be \code{NA} or an empty string,
typically a user input from the Shiny app's UI. Default is an empty string.}
}
\value{
\code{NULL} if the input parameter is \code{NA} or an empty string;
otherwise, the numeric value of the input parameter.
}
\description{
This internal function is used to process input parameters in a Shiny app.
It checks if the provided parameter is \code{NA} or an empty string and accordingly
returns \code{NULL} or converts it to a numeric value. This function ensures that
simulation functions receive properly formatted parameters.
}
\keyword{internal}
