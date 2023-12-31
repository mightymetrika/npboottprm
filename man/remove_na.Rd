% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nonparboot.R
\name{remove_na}
\alias{remove_na}
\title{Remove NA values from vectors}
\usage{
remove_na(na_rm, ...)
}
\arguments{
\item{na_rm}{A logical value indicating whether to remove NA values.}

\item{...}{One or more vectors from which to remove NA values.}
}
\value{
A list of vectors with NA values removed (if \code{na_rm} is \code{TRUE}),
or the input vectors unchanged (if \code{na_rm} is \code{FALSE}).
}
\description{
This function removes NA values from a list of vectors. If na_rm is TRUE,
it removes all NA values from the input vectors. Otherwise, it returns the
input vectors unchanged.
}
\keyword{internal}
