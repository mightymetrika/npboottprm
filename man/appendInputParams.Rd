% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replext_helpers.R
\name{appendInputParams}
\alias{appendInputParams}
\title{Append Input Parameters to Data Frame}
\usage{
appendInputParams(df, input)
}
\arguments{
\item{df}{A data frame containing the simulation results.}

\item{input}{A list of input parameters used in the simulation, typically sourced from the Shiny app's user inputs.}
}
\value{
A data frame that combines the original simulation results with the input parameters used in the simulation.
Additionally, a unique run code is generated and appended to each row for identification purposes.
}
\description{
This internal function appends the input parameters and a unique run code to the data frame of simulation results.
It creates a comprehensive data frame that includes both the results and the parameters used for the simulation,
facilitating easier tracking and analysis of the simulation runs.
}
\keyword{internal}
