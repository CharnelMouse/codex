% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/versus_summaries.r
\name{get_matchups}
\alias{get_matchups}
\title{Get matchup samples data.table for given decks/players}
\usage{
get_matchups(tidy_results, deck_names, players = NULL, player_seed = 1)
}
\arguments{
\item{tidy_results}{A list, containing a versus model's tidy results.}

\item{deck_names}{A character vector, giving standardised deck names to get
matchups for. Standardised names may used the package's default nicknames.}

\item{players}{A character vector, giving players to assign to the given
decks, in order. If NULL, players are not accounted for. Otherwise, the
length must be equal to the length of the deck names. Players not present
in the model information will have their skill samples generated.}

\item{player_seed}{An integer, giving the random seed used to generate
unknown players' skill samples.}
}
\value{
A data.table with four columns: \code{P1} and \code{P2} for the
  decks/players involved, \code{prob_matchup} for the sampled P1 win
  probability, and \code{prob_mean} for the mean P1 win probability over all
  samples for that matchup, pre-calculated for use in
  \code{\link{plot_matchup_samples}}.
}
\description{
Get matchup samples data.table for given decks/players
}
