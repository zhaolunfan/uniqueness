% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.r
\name{generate_bootstrap_samples}
\alias{generate_bootstrap_samples}
\title{Generate Bootstrap Samples}
\usage{
generate_bootstrap_samples(data, I = 1000, size = 200)
}
\arguments{
\item{data}{A data frame containing the data to sample from.}

\item{I}{An integer specifying the number of bootstrap samples to generate. Default is 1000.}

\item{size}{An integer specifying the size of each bootstrap sample. Default is 200.}
}
\value{
A data frame where each column is a bootstrap sample.
}
\description{
This function generates bootstrap samples from the provided data.
}
