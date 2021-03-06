% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nash.r
\name{reformat_used_nash}
\alias{reformat_used_nash}
\title{Reformat Nash table to give results only for used strategies}
\usage{
reformat_used_nash(nash)
}
\arguments{
\item{nash}{A matrix, containing a single Nash equilibrium information table.}
}
\value{
A data.table, with rows for each used strategy and columns for each
  game, sorted for "Both" game.
}
\description{
Reformat Nash table to give results only for used strategies
}
