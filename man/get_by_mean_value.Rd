% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise_results.r
\name{get_by_mean_value}
\alias{get_by_mean_value}
\title{Get parameters by mean sampled value}
\usage{
get_by_mean_value(samples, names = colnames(samples), sort_fn = sort)
}
\arguments{
\item{samples}{a tibble, with a column of sampled values for each model parameter}

\item{names}{a character vector, giving names of parameters to sort, defaults to all parameters}

\item{sort_fn}{a function used to sort the mean parameter values, defaults to sort()}
}
\value{
a character vector, giving parameters sorted in order of mean sampled value.
}
\description{
Get parameters by mean sampled value
}
