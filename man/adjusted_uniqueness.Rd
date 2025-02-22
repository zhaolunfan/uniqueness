% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.r
\name{adjusted_uniqueness}
\alias{adjusted_uniqueness}
\title{Adjusted Uniqueness}
\usage{
adjusted_uniqueness(data, I = 1000, size = 200)
}
\arguments{
\item{data}{A data frame containing the data to sample from.}

\item{I}{An integer specifying the number of bootstrap samples to generate.}

\item{size}{An integer specifying the size of each bootstrap sample.}
}
\value{
A single numeric value representing the adjusted uniqueness score.
}
\description{
This function calculates the adjusted uniqueness score using bootstrap samples.
}
